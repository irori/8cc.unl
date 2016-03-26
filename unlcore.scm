#!/usr/bin/gosh
(add-load-path "." :relative)

(use parser)
(use unlc)
(use lib)
(use unlasm)

(use srfi-60)

;; ALU ---------------------------------------------------------------

(defmacro int-inc
  (lambda (n)
    (let ((lo (succ (low n))))
      (if (< lo c256)
	  (make-int lo (mid n) (high n))
	  (let ((mi (succ (mid n))))
	    (if (< mi c256)
		(make-int c0 mi (high n))
		(make-int c0 c0	; TODO: inline?
			  (let ((hi (succ (high n))))
			    (if< hi c256 hi c0)))))))))

(defmacro int-dec
  (lambda (n)
    (if (nonzero? (low n))
	(make-int (pred (low n)) (mid n) (high n))
	(if (nonzero? (mid n))
	    (make-int c255 (pred (mid n)) (high n))
	    (make-int c255 c255 (ifnonzero (high n) (pred (high n)) c255)))))) ; TODO: inline?

(add-unl-macro! 'add-table '()
  `(clist ,@(map (lambda (n) `(cons ,(churchnum n) I)) (iota 256))
	  ,@(map (lambda (n) `(cons ,(churchnum n) cdr)) (iota 256))))

(defmacro int-add
  (let ((tbl add-table))
    (lambda (x y)
      ((nth (low y) ((low x) cdr tbl))
       (lambda (z1 c1)
	 ((nth (mid y) ((mid x) cdr (c1 tbl)))
	  (lambda (z2 c2)
	    ((nth (high y) ((high x) cdr (c2 tbl)))
	     (lambda (z3 c3)
	       (inline-int z1 z2 z3))))))))))

(add-unl-macro! 'not-table '()
  `(clist ,@(map (lambda (n) (churchnum (- 255 n))) (iota 256))))

(defmacro int-neg
  (let ((tbl not-table))
    (lambda (n)
      (int-inc
       (make-int (nth (low n) tbl)
		 (nth (mid n) tbl)
		 (nth (high n) tbl))))))

;; returns K(true) or KI(false)
(defmacro int-eq
  (lambda (x y)
    (if (= (low x) (low y))
	(if (= (mid x) (mid y))
	    (if (= (high x) (high y)) K KI)
	    KI)
	KI)))

;; returns K(true) or KI(false)
(defmacro int-lt
  (lambda (x y)
    (if (= (high x) (high y))
	(if (= (mid x) (mid y))
	    (if< (low x) (low y) K KI)
	    (if< (mid x) (mid y) K KI))
	(if< (high x) (high y) K KI))))

;; for debug
(defmacro print-int
  (lambda (n)
    (S (print-digit (high n))
       (S #\space
	  (S (print-digit (mid n))
	     (S #\space
		(S (print-digit (low n))
		   #\newline))))
       I)))

;; memory ------------------------------------------------------------

(defmacro zero-line (c256 (cons int-0) nil))

(defmacro zero-page (c256 (cons zero-line) nil))

(defrecmacro (initialize-lines n lst)
  (if (cons1? n)
      (let ((next (c256 cdr lst)))
	(if (pair? next)
	    ((initialize-lines (1-of-1 n) next)
	     (lambda (lines rest)
	       (cons (cons lst lines) rest)))
	    (cons ((cons1-length n) (cons zero-line) nil) nil)))
      (cons nil lst)))

(defrecmacro (initialize-pages n data)
  (if (cons1? n)
      (if (pair? data)
	  ((initialize-lines (to-cons1 c256) data)
	   (lambda (page rest)
	     ((initialize-pages (1-of-1 n) rest)
	      (lambda (pages rest2)
		(cons (cons page pages) rest2)))))
	  (cons ((cons1-length n) (cons zero-page) nil) nil))
      (cons nil data)))

(defmacro (initialize-memory data)
  (car (initialize-pages (to-cons1 c256) data)))

(defmacro load16
  (lambda (mem addr)
    (nth (low addr)
	 (nth (high addr) mem))))

(defmacro load24
  (lambda (mem addr)
    (nth (low addr)
	 (nth (mid addr)
	      (nth (high addr) mem)))))

(defmacro store24
  (lambda (mem addr val)
    (update-nth
     (lambda (page)
       (update-nth
	(lambda (line)
	  (update-nth
	   (lambda (oldval) val)
	   (low addr)
	   line))
	(mid addr)
	page))
     (high addr)
     mem)))

; Runner -------------------------------------------------------------

(defmacro run
  (lambda (code initial-vm)
    (let rec ((vm initial-vm))
      (let* ((pc (vm-pc vm))
	     (f (load24 code pc)))
	(rec (f (set-pc vm (int-inc pc))))))))

; Runtime library ----------------------------------------------------

(defmacro (mem-load vm addr)
  (load24 (vm-memory vm) addr))

(defmacro (mem-store vm addr val)
  (set-memory vm (store24 (vm-memory vm) addr val)))

;; TODO: make putc / getc 8-bit clean
(add-unl-macro! 'putc-table '()
  `(list ,@(map integer->char (iota 128)))) ; Only supports characters up to 127

(defmacro putc
  (lambda (c)
    (nth (low c) putc-table I)))

(add-unl-macro! 'getc '()
  `(lambda (cont)
     (@ I
	(,@(map (lambda (i)
		  `((? ,(integer->char i)) I cont ,(churchnum i)))
		(filter
		 (lambda (i)
		   (let ((c (integer->char i)))
		     (or (char-whitespace? c) (not (eq? (char-general-category c) 'Cc)))))
		 (iota 128)))
	 (cont c0)))))

(defmacro lib (clist int-inc int-dec int-add int-neg int-eq int-lt mem-load mem-store putc getc))

(defmacro test-code (cons* (lambda (vm) (mem-store vm (make-int c0 c0 c1) (make-int c3 c2 c1)))
			   (lambda (vm) (K vm (print-int (lib-load vm vm (make-int c0 c0 c1)))))
			   (lambda (vm) (K vm (print-int (vm-pc vm))))
			   (lambda (vm) (K vm (print-int (vm-pc vm))))
			   (lambda (vm) (K vm (print-int (vm-pc vm))))
			   (c256 (cons (lambda (vm) (exit I))) nil)))

(defmacro (initial-vm data)
  (cons initial-regs
	(cons (initialize-memory data)
	      lib)))

(defmacro main
  (lambda (code data)
    (run (initialize-memory code) (initial-vm data))))

(define (main args)
  (let ((parsed (parse)))
    (print "``")
    (print "# VM core")
    (print-as-unl 'main)
    (newline)
    (generate (car parsed) (cadr parsed))
    0))
