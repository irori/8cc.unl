#!/usr/bin/gosh

(add-load-path "." :relative)

(define-module unlasm
  (use parser)
  (use unlc)
  (use lib)
  (use srfi-1)
  (export defmacro16 defmacro24 vm-number generate)
  )
(select-module unlasm)

(define-macro (defmacro16 name-args body)
  (if (not bfs24?)
      `(defmacro ,name-args ,body)))

(define-macro (defmacro24 name-args body)
  (if bfs24?
      `(defmacro ,name-args ,body)))

;; Integer -----------------------------------------------------------

(defmacro16 make-int (lambda (lo hi f) (f lo hi)))
(defmacro16 make-byte (lambda (b f) (f b c0)))
(defmacro16 (inline-int lo hi) (lambda (f) (f lo hi)))
(defmacro16 (low n)  (n (lambda (x _) x)))
(defmacro16 (high n) (n (lambda (_ x) x)))
(defmacro16 int-0 (inline-int c0 c0))

(defmacro24 make-int (lambda (lo mi hi f) (f lo mi hi)))
(defmacro24 make-byte (lambda (b f) (f b c0 c0)))
(defmacro24 (inline-int lo mi hi) (lambda (f) (f lo mi hi)))
(defmacro24 (low n)  (n (lambda (x _ _) x)))
(defmacro24 (mid n)  (n (lambda (_ x _) x)))
(defmacro24 (high n) (n (lambda (_ _ x) x)))
(defmacro24 int-0 (inline-int c0 c0 c0))

(define (vm-number n)
  (cons 'inline-int
	(map (lambda (sft)
	       (churchnum (logand #xff (ash n sft))))
	     (if bfs24?
		 (list 0 -8 -16)
		 (list 0 -8)))))

; VM state -----------------------------------------------------------

; Register order: PC, A, B, C, D, BP, SP
(defmacro num-regs c7)
(defmacro initial-regs
  (num-regs (cons int-0) nil))

(defmacro (replace-car lst x) (cons x (cdr lst)))

(defmacro (vm-regs vm) (car vm))
(defmacro (set-reg vm regno val)
  (replace-car vm (update-nth (K val) regno (vm-regs vm))))
(defmacro (vm-memory vm) (cadr vm))
(defmacro (set-memory vm mem) (cons (car vm)
				    (cons mem (cddr vm))))

(defmacro (vm-pc vm)
  (car (vm-regs vm)))
(defmacro (set-pc vm pc)
  (replace-car vm (replace-car (vm-regs vm) pc)))

;; VM's third element and later are library routines.
(defmacro lib-inc (nth c2))
(defmacro lib-dec (nth c3))
(defmacro lib-add (nth c4))
(defmacro lib-neg (nth c5))
(defmacro lib-eq (nth c6))
(defmacro lib-lt (nth c7))
(defmacro lib-load (nth c8))
(defmacro lib-store (nth c9))
(defmacro lib-putc (nth c10))
(defmacro lib-getc (nth c11))

; Compiler -----------------------------------------------------------

(define (regpos reg)
  (define regs '((pc . 0) (a . 1) (b . 2) (c . 3) (d . 4) (bp . 5) (sp . 6)))
  (churchnum (cdr (assq reg regs))))

(define (generate-reg-or-simm x)
  (if (symbol? x)
      `(nth ,(regpos x) (vm-regs vm))
      (vm-number x)))

(define (generate-op op args)
  (cond ((eq? op 'mov)
	 (let ((dst (regpos (first args)))
	       (val (generate-reg-or-simm (second args))))
	   `(set-reg vm ,dst ,val)))
	((and (eq? op 'add) (eq? 1 (second args)))
	 (let ((dst (regpos (first args)))
	       (val (generate-reg-or-simm (first args))))
	   `(set-reg vm ,dst (lib-inc vm ,val))))
	((or (and (eq? op 'sub) (eq? (second args) 1))
	     (and (eq? op 'add) (eq? (second args) (- (ash 1 vm-bits) 1))))
	 (let ((dst (regpos (first args)))
	       (val (generate-reg-or-simm (first args))))
	   `(set-reg vm ,dst (lib-dec vm ,val))))
	((eq? op 'add)
	 (let ((dst (regpos (first args)))
	       (lhs (generate-reg-or-simm (first args)))
	       (rhs (generate-reg-or-simm (second args))))
	   `(set-reg vm ,dst (lib-add vm ,lhs ,rhs))))
	((eq? op 'sub)
	 (let ((dst (regpos (first args)))
	       (lhs (generate-reg-or-simm (first args)))
	       (rhs (generate-reg-or-simm (second args))))
	   `(set-reg vm ,dst (lib-add vm ,lhs (lib-neg vm ,rhs)))))
	((eq? op 'jmp)
	 (let ((addr (generate-reg-or-simm (first args))))
	   `(set-reg vm ,(regpos 'pc) ,addr)))
	((memq op '(jeq jne jlt jgt jle jge))
	 (let ((addr (generate-reg-or-simm (first args)))
	       (lhs (generate-reg-or-simm (second args)))
	       (rhs (generate-reg-or-simm (third args))))
	   (let ((cmp (if (memq op '(jeq jne)) 'lib-eq 'lib-lt))
		 (flip? (memq op '(jle jgt)))
		 (not? (memq op '(jne jle jge)))
		 (set-pc `(set-reg vm ,(regpos 'pc) ,addr)))
	     `((,cmp vm ,(if flip? rhs lhs) ,(if flip? lhs rhs))
	       ,(if not? 'vm set-pc)
	       ,(if not? set-pc 'vm)))))
	((memq op '(eq ne lt gt le ge))
	 (let ((dst (regpos (first args)))
	       (lhs (generate-reg-or-simm (first args)))
	       (rhs (generate-reg-or-simm (second args))))
	   (let ((cmp (if (memq op '(eq ne)) 'lib-eq 'lib-lt))
		 (flip? (memq op '(le gt)))
		 (not? (memq op '(ne le ge))))
	     (let ((val `(,cmp vm ,(if flip? rhs lhs) ,(if flip? lhs rhs))))
	       `(set-reg vm ,dst
			 (make-byte (,val ,(if not? 'c0 'c1)
					  ,(if not? 'c1 'c0))))))))
	((eq? op 'load)
	 (let ((dst (regpos (first args)))
	       (addr (generate-reg-or-simm (second args))))
	   `(set-reg vm ,dst (lib-load vm vm ,addr))))
	((eq? op 'store)
	 (let ((src (generate-reg-or-simm (first args)))
	       (addr (generate-reg-or-simm (second args))))
	   `(lib-store vm vm ,addr ,src)))
	((eq? op 'putc)
	 (let ((val (generate-reg-or-simm (first args))))
	   `(K vm (lib-putc vm ,val))))
	((eq? op 'getc)
	 (let ((dst (regpos (first args))))
	   `(set-reg vm ,dst (make-byte (call/cc (lib-getc vm))))))
	((eq? op 'exit)
	 '(exit I))))

(define (compile-chunk chunk)
  (reduce (lambda (inst rest)
	    (list 'compose inst rest))
	  'I
	  (map (lambda (inst) `(lambda (vm) ,(generate-op (car inst) (cdr inst))))
	       chunk)))

(define (compile-code code)
  (cons 'clist (append (map (compose compile-to-string compile-chunk) code)
		       (map (lambda (_) 'I) (iota 256)))))

(define (initial-data data)
  `((c256 (cons int-0))
    ,(cons 'clist
	   (map (compose compile-to-string vm-number)
		(append data (map (lambda (_) 0) (iota 256)))))))

(define (generate code data)
  (print "# instructions")
  (print-as-unl (compile-code code))
  (newline)
  (print "# data")
  (print-as-unl (initial-data data))
  (newline)
  0)

;; usage: gosh -m unlasm unlasm.scm
(define (main args)
  (let* ((code (read))
	 (data (read)))
    (generate code data)))
