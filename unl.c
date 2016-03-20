/************************* UNLAMBDA INTERPRETER *****************************/
/*
  (c) 2004, 2006 Emil Jerabek
  This is free software. You may modify and/or distribute it under the terms
  of the GNU General Public License, version 2 or any later version.
  It comes with no warranty.

  History & credits:
  Originally this started as a fork of J. Mandelson's interpreter, but it
  soon turned out that it is easier to reimplement it from the scratch.
  Some ideas from J. Mandelson's interpreter are still used, though.

  Main new ideas:
  - call stack is a linked list, whose elements are reference counted and
    garbage collected as other structures
  - custom allocation routine, to avoid malloc() overhead

  CHANGELOG:
  2004-??-??: initial release
  2006-02-24: new option -u, support for multiple -e, modified dump()
  2006-03-01: fixed 8-bit input in -e

*/

#ifndef COMPILE
#define _POSIX_C_SOURCE 2 /* getopt() */
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#endif
#include <stdio.h>
#include <stdlib.h>

/**************** ERRORS, INTERPRETER DEBUGGING ****************/
static char *progname = NULL;

#ifdef __GLIBC__
#include <error.h>
#else
#include <stdarg.h>
static void error (int exit_code, int dummy, char *format, ...)
{
  va_list varg;

  fprintf (stderr, "%s: ", progname);
  va_start (varg, format);
  vfprintf (stderr, format, varg);
  va_end (varg);
  putc ('\n', stderr);
  if (exit_code) exit (exit_code);
}
#endif

/* BTW, the interpreter exit code is:
    0 - OK
    1 - command-line or parse error
    2 - insufficient virtual memory
    3 - BUG
*/

#if (DEBUG - 0) & 0x01
#  define MEMCHECK 1
#  define MEM(X) X
#else
#  define MEM(X)
#endif

static int fail = 0;

#ifdef DEBUG
#  define assert(W, X) do if (!(X)) {fprintf (stderr, "%s", W ": assertion " #X " failed\n"); fail = 3;} while (0)
#else
#  define assert(W, X)
#endif

/********************* DATA STRUCTURES *********************/

typedef enum {
  Back, S2, D2,
  S1, K1, D1,
  C1,
  S, K, I, V, C, D, E, Dot, Q, At, Pipe, Slash
} ftype;

typedef enum { /* Ev0,*/ Ev1, Ev2, /* Ap,*/ ApD, ApS, Done } atype;

struct _stack;

/* functions and expressions */
#if (__GNUC__ - 0) >= 2 && (__GNUC__ >= 3 || __GNUC_MINOR__ >= 96)
typedef struct _func {
  long rc;
  ftype t;
  union {
    struct { struct _func *l, *r; };
    int c;
    struct _stack *st;
  };
} func;
#define IC(X) {.c = X}
#else
typedef struct _func {
  long rc;
  ftype t;
  union {
    struct { struct _func *L, *R; } LR;
    int C;
    struct _stack *S;
  } U;
} func;
/* Terrible hack. I should have realized earlier that unnamed struct fields
   are GCC extension. */
#define c U.C
#define st U.S
#define l U.LR.L
#define r U.LR.R
#define IC(X) {.C = X}
/* If this does not work (i.e., the compiler does not understand ISO C99
   labeled union initializers), uncomment the line below, and prey.
   Alternatively, you may
    - find a way of doing this portably in pure ANSI C, in which case please
      let me know
    - swap the members LR and C, and do not use unlc
*/
/* #define IC(X) {{(func *) X}} */
#endif

/* T:
    S1 - function `sL
    S2 - function ``sLR
    K1 - function `kL
    D1 - promise  `dL (L is an expression)
    D2 - promise  `d`LR (L and R are functions)
    C1 - continuation (S is the stack entry to be resumed)
    Back - expression `LR
The rest are basic functions/expressions.
Promises (D1) are never created for evaluated functions, except for d itself.
The allocator reuses L as the link to the next free object. */

/* stack entries */
typedef struct _stack {
  long rc;
  struct _stack *n;
  atype t;
  func *a, *b;
} stack;
/* A and B are arguments and/or intermediate values.
   The result is passed in variable res, local to run().
   N is a link to the next stack entry.
   T is the type of the entry:
    Ev0 - evaluate expression A
          >  Ev0->Ev1 or Ev1 (if B simple)
    Ev1 - evaluate `<res>A, res already evaluated
          >  Ev0->Ev2 or Ap (if A simple) or pop (if <res>==D)
    Ev2 - apply `A<res>
          >  Ap
    Ap  - apply `AB
          >  pop (a->t is S, K, S1, K1, Dot, I, D, V)
	  or Ap (C, Q, At, Pipe, Slash, D2)
	  or resume cont B (C1)
	  or return B (E)
	  or Ev0->ApD (D1)
	  or Ap->ApD (D2)
	  or Ap->ApS (S2)
    ApD - apply `<res>B
          >  Ap
    ApS - evaluate `<res>`AB (all three already evaluated)
          >  Ap->Ev2 or pop (if <res>==D)
    Done - return <res>
Ev0 is never applied to simple functions, these cases are inlined.
The top of the current stack may have incomplete information, as
goto is often used instead of filling the T slot.
Ev0 and Ap may appear only on top of the current stack, with reference count 1.
In fact, now they are always handled by goto, and thus they do not officially
exist in the atype enum.
The allocator uses N as the link field. */

#ifndef COMPILE
void dumpc (FILE *F, stack *s);

void dump (FILE *F, func *f)
{
  switch (f->t) {
    case Back: putc ('`', F); dump (F, f->l); dump (F, f->r); break;
    case S2: fputs ("s(", F); dump (F, f->l);
             putc (',', F); dump (F, f->r);
	     putc (')', F);
	     break;
    case D2: fputs ("d(`", F); dump (F, f->l); dump (F, f->r); putc (')', F);
	     break;
    case S1: fputs ("s(", F); dump (F, f->l); putc (')', F); break;
    case K1: fputs ("k(", F); dump (F, f->l); putc (')', F); break;
    case D1: fputs ("d(", F); dump (F, f->l); putc (')', F); break;
    case C1: fputs ("c(", F); dumpc (F, f->st); putc (')', F); break;
    case S:  putc ('s', F); break;
    case K:  putc ('k', F); break;
    case I:  putc ('i', F); break;
    case V:  putc ('v', F); break;
    case C:  putc ('c', F); break;
    case D:  putc ('d', F); break;
    case E:  putc ('e', F); break;
    case At: putc ('@', F); break;
    case Pipe: putc ('|', F); break;
    case Slash: putc ('/', F); break;
    case Dot: if (f->c == '\n')
                putc ('r', F);
              else {
		putc ('.', F);
		putc (f->c, F);
              }
              break;
    case Q: putc ('?', F); putc (f->c, F); break;
    default: error (3, 0, "dump(): unrecognized expression type %d", f->t);
  }
}

void dumpc (FILE *F, stack *s)
{
  while (s) {
    switch (s->t) {
      case Ev1:  fputs ("EV `#", F); dump (F, s->a); putc (';', F); break;
      case Ev2:  fputs ("AP ", F);   dump (F, s->a); putc (';', F); break;
      case ApD:  fputs ("EV `#", F); dump (F, s->b); putc (';', F); break;
      case ApS:  fputs ("EV `#`", F);
	         dump (F, s->a); dump (F, s->b);
		 putc (';', F); break;
      case Done: fputs ("RET", F); break;
      default: error (3, 0, "dumpc(): unrecognized instruction %d", s->t);
    }
    s = s->n;
  }
}
#endif

/******************* MEMORY MANAGEMENT **********************/

/* To avoid frequent malloc/free calls, we use a simple inlined allocation
strategy: we maintain linked lists of free func and stack objects. */
static func *flist = NULL;
static stack *slist = NULL;

#ifdef MEMCHECK
static long newf_c = 0, freef_c = 0, chunkf_c = 0;
static long news_c = 0, frees_c = 0, chunks_c = 0;
#endif

/* allocate a new func/stack into X */
#define newf(X) do { MEM(newf_c++;) if (!flist) growf ();\
           (X) = flist; (X)->rc = 1; flist = flist->l;\
           MEM((X)->l=(X)->r=NULL;(X)->t=-1;)} while (0)
#define newS(X) do { stack *_x; MEM(news_c++;) if (!slist) grows ();\
           _x = slist; _x->rc = 1; slist = slist->n; (X) = _x;\
           MEM(_x->a=_x->b=NULL;_x->n=NULL;_x->t=-1;)} while (0)
/* shortcuts, initialize X->n and/or X->t as well; this is handy
for pushing on the stack: newSS (s, s) */
/* the naming of newS/newSS/news is messy for historic reasons,
but you can live with that I hope */
#define newSS(X, N) do { stack *_x; MEM(news_c++;) if (!slist) grows ();\
           _x = slist; _x->rc = 1; slist = slist->n; _x->n = (N); (X) = _x;\
           MEM(_x->a=_x->b=NULL;_x->t=-1;)} while (0)
#define news(X, T, N) do { stack *_x; MEM(news_c++;) if (!slist) grows ();\
           _x = slist; _x->rc = 1; _x->t = (T);\
           slist = slist->n; _x->n = (N); (X) = _x;\
           MEM(_x->a=_x->b=NULL;)} while (0)
/* take a "copy" of a func/stack */
#define clonef(X) ((X)->rc++, (X))
#define clones(X) ((X)->rc++, (X))

/* free a func/stack */
#define freef(X) do { MEM(freef_c++;) (X)->l = flist; flist = (X); } while (0)
#define frees(X) do { MEM(frees_c++;) (X)->n = slist; slist = (X); } while (0)
/* delete a func/stack reference, w/ garbage collection */
#define delf(X) do { if (!--((X)->rc)) Freef(X); } while (0)
#define dels(X) do { if (!--((X)->rc)) Frees(X); } while (0)
/* pop a stack entry */
#define pops(X) do { stack *_x = (X); (X) = _x->n;\
           if (--(_x->rc)) (X)->rc++;\
           else frees (_x); } while (0)
#define popsa(X) do { stack *_x = (X); (X) = _x->n;\
           if (--(_x->rc)) { (X)->rc++; _x->a->rc++; }\
           else frees (_x); } while (0)
#define popsab(X) do { stack *_x = (X); (X) = _x->n;\
           if (--(_x->rc)) { (X)->rc++; _x->a->rc++; _x->b->rc++;}\
           else frees (_x); } while (0)

/* free a func/stack, and delete the objects it references */

static void Frees (stack *s);

static void Freef (func *f)
{
  MEM(if (f->t == -1) error (3, 0, "invalid type in Freef()");)
  switch (f->t) {
    case Back: case S2: case D2:
      delf (f->r);
    case S1: case K1: case D1:
      delf (f->l); break;
    case C1:
      dels (f->st);
    default:;
  }
  freef (f);
}

/* Note:
There is only 1 stack entry of type Done, and it is never Free'd (unless
we do memory checking). In principle we could get rid of this entry altogether,
replacing it with NULL, but it makes life easier, as its ref count is accessed
in several places through s->n. */
static void Frees (stack *s)
{
  switch (s->t) {
#ifdef MEMCHECK
    case Done:
      assert ("Frees", !s->n); frees (s); return;
#endif
    default: error (3, 0, "invalid type %d in Frees()", s->t);
    case ApD:
      delf (s->b); break;
    case ApS:
      delf (s->b);
    case Ev1: case Ev2:
      delf (s->a);
  }
  dels (s->n);
  frees (s);
}

/* When we run out of free func/stack objects, we allocate some more
through the system malloc(), and put them on the free list.
We could easily bypass malloc() altogether via brk/sbrk, but it doesn't save
anything, and it is not POSIX. */

static void out_of_memory (char *type)
{
  /* error (2, 0, "out of memory (%s)", type); */
  /* On my system this call segfaults. Probably fprintf() or some other
     guy called from error() tries to allocate memory. Grrr */
  fputs (progname, stderr);
  fputs (": out of memory (", stderr);
  fputs (type, stderr);
  fputs (")\n", stderr);
  exit (2);
}

#define CHUNKSIZE 1020

static void growf (void)
{
  int i;
  func *p = flist = (func*) malloc (CHUNKSIZE * sizeof (func));
  if (!p) out_of_memory ("functions/expressions");
  for (i = 0; i < CHUNKSIZE - 1; i++, p++) p->l = p + 1;
  p->l = NULL;
  MEM(chunkf_c++;)
}

static void grows (void)
{
  int i;
  stack *p = slist = (stack*) malloc (CHUNKSIZE * sizeof (stack));
  if (!p) out_of_memory ("call stack");
  for (i = 0; i < CHUNKSIZE - 1; i++, p++) p->n = p + 1;
  p->n = NULL;
  MEM(chunks_c++;)
}

/*************************** PARSER ******************************/

static func preI;
static func preV;

#ifndef COMPILE
static func preS = {1, S};
static func preK = {1, K};
static func preI = {1, I};
static func preV = {1, V};
static func preC = {1, C};
static func preD = {1, D};
static func preE = {1, E};
static func preAt = {1, At};
static func prePipe = {1, Pipe};
static func preSlash = {1, Slash};
static func preR = {1, Dot, IC('\n')};
/* if you get a compilation error here, look at the definition of IC near
   the beginning of the file */

static FILE *program = NULL;
static unsigned char *script = NULL;

/* The Unlambda program comes either from an I/O stream (file or stdin),
or from a string. getnext() reads the next character from the appropriate
source, bypassing whitespace and comments (unless dot_or_ques is nonzero).
Comments are _not_ recognized in a string expression. */
static int getnext (int dot_or_ques)
{
  int ch;
  do {
    ch = program ? getc (program) : *script ? *script++ : EOF;
    if (ch == EOF) error (1, 0, "unexpected EOF");
    if (dot_or_ques) break;
    if (ch == '#' && program)
      do
	if ((ch = getc (program)) == EOF) error (1, 0, "unexpected EOF");
      while (ch != '\n');
  } while (isspace (ch));
  return ch;
}

/* the parser itself */
func *parse (void)
{
  int ch;
  for (;;)
    switch (ch = getnext (0)) {
      case 's': case 'S': return clonef (&preS);
      case 'k': case 'K': return clonef (&preK);
      case 'i': case 'I': return clonef (&preI);
      case 'v': case 'V': return clonef (&preV);
      case 'c': case 'C': return clonef (&preC);
      case 'd': case 'D': return clonef (&preD);
      case 'e': case 'E': return clonef (&preE);
      case 'r': case 'R': return clonef (&preR);
      case '@': return clonef (&preAt);
      case '|': return clonef (&prePipe);
      case '/': return clonef (&preSlash);
      case '.': case '?': case '`':
	{
	  func *res;
	  newf (res);
	  if (ch == '`') {
	    res->t = Back;
	    res->l = parse ();
	    res->r = parse ();
	  } else {
	    res->t = ch == '.' ? Dot : Q;
	    res->c = getnext (1);
	  }
	  return res;
	}
      default:  error (0, 0, "unrecognized character `%c' ignored", ch);
    }
}

/******************** INTERPRETER PROPER **********************/

static int debug = 0; /* show progress */
static FILE *input; /* Unlambda input */
#else
/* This "#else" belongs to "#ifndef COMPILE" around the parser. */
/* In "compiled" mode, we don't support input redirection. */
#  define input stdin
#endif

/* Output redirection dismissed as redundant, use your shell. */
#define output stdout


func *run (func *res)
{
  int cur_ch = EOF; /* Unlambda "current character" */
  stack *s; /* top of the current stack */
  if (res->t != Back) return res;
  news (s, Done, NULL);
  newSS (s, s);
  s->a = res;
  goto cEv0;

  for (;;)
    switch (s->t) {
      case Done:
	MEM(dels (s);)
	return res;
	/* case Ev0: */
    cEv0:
	assert ("Ev0", s->rc == 1);
	assert ("Ev0", s->a->t == Back);
	{
	  stack *tmps;
	  func *tmpf = s->a->l;
	  if (tmpf->t != Back) {
	    res = tmpf;
	    tmpf = s->a->r;
	    if (--(s->a->rc)) { res->rc++; tmpf->rc++; }
	    else freef (s->a);
	    s->a = tmpf;
	    goto cEv1;
	  }
	  news (tmps, Ev1, s->n);
	  tmps->a = s->a->r;
	  if (--(s->a->rc)) { tmpf->rc++; tmps->a->rc++; }
	  else freef (s->a);
	  s->n = tmps;
	  s->a = tmpf;
	  goto cEv0;
	}
      case Ev1:
    cEv1:
	if (res->t == D) {
	  res->rc--;/*???*/
	  if (s->a->t != Back && s->a->t != D) res = s->a;
	  else {
	    newf (res);
	    res->t = D1;
	    res->l = s->a;
	  }
	  popsa (s);
	  continue;
	} else if (s->a->t != Back) {
	  if (s->rc == 1) s->b = s->a;
	  else {
	    func *tmpf = clonef (s->a);
	    s->rc--;
	    s->n->rc++;
	    newSS (s, s->n);
	    s->b = tmpf;
	  }
	  s->a = res;
	  goto cAp;
	} else {
	  stack *tmps;
	  news (tmps, Ev2, s->n);
	  tmps->a = res;
	  if (s->rc > 1) {
	    func *tmpf = clonef (s->a);
	    s->rc--;
	    s->n->rc++;
	    newS (s);
	    s->a = tmpf;
	  }
	  s->n = tmps;
	  goto cEv0;
	}
      case Ev2:
	if (s->rc > 1) {
	  stack *tmps;
	  s->rc--;
	  newSS (tmps, clones (s->n));
	  tmps->a = clonef (s->a);
	  s = tmps;
	}
	s->b = res;
	goto cAp;
      case ApD:
	if (s->rc > 1) {
	  stack *tmps;
	  s->rc--;
	  newSS (tmps, clones (s->n));
	  tmps->b = clonef (s->b);
	  s = tmps;
	}
	s->a = res;
	goto cAp;
      case ApS:
	if (res->t == D) {
	  res->rc--;/*???*/
	  newf (res);
	  res->t = D2;
	  res->l = s->a;
	  res->r = s->b;
	  popsab (s);
	  continue;
	} else {
	  stack *tmps;
	  news (tmps, Ev2, s->n);
	  tmps->a = res;
	  if (s->rc > 1) {
	    func *tmpfa = clonef (s->a);
	    func *tmpfb = clonef (s->b);
	    s->rc--;
	    s->n->rc++;
	    newS (s);
	    s->a = tmpfa;
	    s->b = tmpfb;
	  }
	  s->n = tmps;
	  /* goto cAp; */
	}
	/* case Ap: */
    cAp:
	{
	  func *a = s->a, *b = s->b;
	  assert ("Ap", s->rc == 1);
#ifndef COMPILE
	  if (debug) {
	    /* beware that some Ap calls are "inlined" */
	    fflush (output);
	    fputs ("applying ", stderr);
	    dump (stderr, a);
	    fputs (" to ", stderr);
	    dump (stderr, b);
	    putc ('\n', stderr);
	  }
#endif
	cAp2:
	  switch (a->t) {
	    case S:
	      a->rc--;/*???*/
#ifdef OPTV
	      if (b->t == V) res = b;
	      else
#endif
	      {
		newf (res);
		res->t = S1;
		res->l = b;
	      }
	      pops (s); continue;
	    case K:
	      a->rc--;/*???*/
#ifdef OPTV
	      if (b->t == V) res = b;
	      else
#endif
	      {
		newf (res);
		res->t = K1;
		res->l = b;
	      }
	      pops (s); continue;
	    case S1:
	      if (a->rc == 1) res = a;
	      else {
		newf (res);
		res->l = clonef (a->l);
		a->rc--;
	      }
	      res->t = S2;
	      res->r = b;
	      pops (s); continue;
	    case K1:
	      res = a->l;
	      delf (b);
	      if (--(a->rc)) res->rc++;
	      else freef (a);
	      pops (s); continue;
	    case Dot:
	      putc (a->c, output);
	      res = b;
	      delf (a);
	      pops (s); continue;
	    case I:
	      res = b;
	      a->rc--;/*???*/
	      pops (s); continue;
	    case D:
	      if (b->t != D) {
		res = b;
		a->rc--;/*???*/
	      } else {
		newf (res);
		res->t = D1;
		res->l = a;
	      }
	      pops (s); continue;
	    case V:
	      res = a;
	      delf (b);
	      pops (s); continue;
	    case C:
	      a->rc--;/*???*/;
#ifdef OPTV
	      if (b->t == V) {
		res = b;
		pops (s); continue;
	      }
#endif
	      s->a = a = b;
	      newf (b);
	      b->t = C1;
	      b->st = clones (s->n);
	      s->b = b;
	      goto cAp2;
	    case C1:
	      {
		stack *tmps = a->st;
		res = b;
		if (--(a->rc)) tmps->rc++;
		else freef (a);
		dels (s->n); frees (s);
		s = tmps; continue;
	      }
	    case E:
	      MEM(b->rc++; dels (s);)
              return b;
	    case Q:
	      s->b = a->c == cur_ch ? &preI : &preV;
	      s->b->rc++;/*!!!*/
	      delf (a);
	      s->a = a = b;
	      b = s->b;
	      goto cAp2;
	    case At:
	      a->rc--;/*???*/
	      s->a = a = b;
	      s->b = b = (cur_ch = getc (input)) == EOF ? &preV : &preI;
	      b->rc++;/*!!!*/
	      goto cAp2;
	    case Pipe:
	      a->rc--;/*???*/
	      s->a = a = b;
	      if (cur_ch == EOF) s->b = b = clonef (&preV);/*!!!*/
	      else {
		newf (b);
		b->t = Dot;
		b->c = cur_ch;
		s->b = b;
	      }
	      goto cAp2;
	    case Slash:
	      a->rc--;/*???*/
	      s->a = a = b;
	      if (cur_ch == EOF) s->b = b = clonef (&preV);/*!!!*/
	      else {
		newf (b);
		b->t = Q;
		b->c = cur_ch;
		s->b = b;
	      }
	      goto cAp2;
	    case D1:
	      if (a->l->t != Back) {
		assert ("D1", a->l->t == D);
		if (b->t != D) {
		  res = b;
		  delf (a);
		} else {
		  res = a;
		  b->rc--;/*???*/
		}
		pops (s); continue;
	      }
	      s->t = ApD;
	      newSS (s, s);
	      s->a = a->l;
	      if (--(a->rc)) a->l->rc++;
	      else freef (a);
	      goto cEv0;
	    case D2:
	      s->t = ApD;
	      newSS (s, s);
	      s->a = a->l;
	      s->b = b = a->r;
	      if (--(a->rc)) { a->l->rc++; a->r->rc++; }
	      else freef (a);
	      a = s->a;
	      goto cAp2;
	    case S2:
	      s->t = ApS;
	      s->a = a->r;
	      newSS (s, s);
	      s->a = a->l;
	      s->b = clonef (b);
	      if (--(a->rc)) { a->l->rc++; a->r->rc++; }
	      else freef (a);
	      a = s->a;
	      goto cAp2;
	    default: error (3, 0, "Ap: unrecognized type %d of left operand", a->t);
	  }
	}
      default: error (3, 0, "run(): unrecognized instruction %d", s->t);
    }
}

/***************************** C MAIN *****************************/

#ifdef COMPILE
static func *main_expr;

int main (int argc, char **argv)
{
  progname = argv[0];
  run (main_expr);
  return fail;
}
#else

static void help (int exit_code)
{
  fprintf (stderr, "Usage: %s [-h] [-d] [-u] [-i input] [-e expression | sourcefile]\n", progname);
  exit (exit_code);
}

/* string concatenation in portable assembler */
/* didn't bother to make it efficient, the data type representing strings
   in this language is inherently brain-damaged */
void append (const char *e)
{
  size_t sa, sb;

  sb = strlen (e);
  if (script) {
    sa = strlen (script);
    script[sa++] = '\n';
    script = (char *) realloc (script, sa + sb + 1);
  } else {
    /* NB: strdup is not ANSI C */
    sa = 0;
    script = (char *) malloc (sb + 1);
  }
  strcpy (script + sa, e);
}

int main (int argc, char **argv)
{
  func *expr;
  int ch, close_source = 0, bad_opt = 0, unparse = 0;
  char *inp = NULL, *scr;

  progname = argv[0];
  while ((ch = getopt (argc, argv, "i:e:hdu")) != EOF)
    switch (ch) {
      case 'e': append (optarg); break;
      case 'i': inp = optarg; break;
      case 'h': help (0);
      case 'd': debug++; break;
      case 'u': unparse++; break;
      default: bad_opt++;
    }
  if (bad_opt) help (1);

  scr = script;

  if (scr) {
    if (optind != argc) error (1, 0, "extra argument");
  } else {
    if (optind < argc - 1) error (1, 0, "extra argument");
    if (optind == argc)
      program = stdin;
    else {
      program = fopen (argv[optind], "r");
      if (!program)
	error (1, 0, "cannot open source file `%s'", argv[optind]);
      close_source = 1;
    }
  }
  if (inp) {
    input = fopen (inp, "r");
    if (!input)
      error (1, 0, "cannot open input file `%s'", inp);
  } else input = stdin;

  expr = parse ();
  if (close_source) fclose (program);
  else if (scr)
    free (scr);
  else
    /* if both program and input are from stdin, we do not close it,
       but we discard the rest of the current line first, for convenience */
    do ch = getc (stdin); while (ch != EOF && ch != '\n');

  expr = run (expr);

  if (inp) fclose (input);

  if (unparse) {
    fflush (output);
    fputs ("Result: ", stderr);
    dump (stderr, expr);
    putc ('\n', stderr);
  }

#ifdef MEMCHECK
  delf (expr);
  fflush (output);
  if (newf_c != freef_c || news_c != frees_c) fprintf (stderr, "BANG!!!\n");
  fprintf (stderr,
    "newf:  %ld\t news:  %ld\t chunkf: %ld\t chunks: %ld\nfreef: %ld\t frees: %ld\n",
    newf_c, news_c, chunkf_c, chunks_c, freef_c, frees_c);
#endif

  return fail;
}
#endif
