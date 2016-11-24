#!/usr/bin/gosh
(add-load-path "." :relative)

(use parser)
(use unlc)
(use lib)
(use unlasm)

(use srfi-60)

;; little endian binary number ---------------------------------------

(defmacro add-table
  (icons
   (icons
    (icons
     (lambda (rest) ((icons K) (rest K)))
     (lambda (rest) ((icons KI) (rest K))))
    (icons
     (lambda (rest) ((icons KI) (rest K)))
     (lambda (rest) ((icons K) (rest KI)))))
   (icons
    (icons
     (lambda (rest) ((icons KI) (rest K)))
     (lambda (rest) ((icons K) (rest KI))))
    (icons
     (lambda (rest) ((icons K) (rest KI)))
     (lambda (rest) ((icons KI) (rest KI)))))))

(defrecmacro (le-add-rec xs ys carry)
  (add-table (car xs) (car ys) carry
	     (le-add-rec (cdr xs) (cdr ys))))

(defmacro (le-add xs ys) (le-add-rec xs ys KI))

(defmacro sub-table
  (icons
   (icons
    (icons
     (lambda (rest) ((icons K) (rest K)))
     (lambda (rest) ((icons KI) (rest KI))))
    (icons
     (lambda (rest) ((icons KI) (rest KI)))
     (lambda (rest) ((icons K) (rest KI)))))
   (icons
    (icons
     (lambda (rest) ((icons KI) (rest K)))
     (lambda (rest) ((icons K) (rest K))))
    (icons
     (lambda (rest) ((icons K) (rest K)))
     (lambda (rest) ((icons KI) (rest KI)))))))

(defrecmacro (le-sub-rec xs ys borrow)
  (sub-table (car xs) (car ys) borrow
	     (le-sub-rec (cdr xs) (cdr ys))))

(defmacro (le-sub xs ys) (le-sub-rec xs ys KI))

(defmacro eq-table
  (icons
   (icons I (K (K (K I))))
   (icons (K (K (K I))) I)))

;; returns K(true) or KI(false)
(defrecmacro (le-eq-rec xs ys)
  (eq-table (car xs) (car ys) le-eq-rec (cdr xs) (cdr ys)))

(defmacro (le-eq xs ys)
  (if (le-eq-rec xs ys) KI K))

(defmacro borrow-table
  (icons
   (icons I (K KI))
   (icons (K K) I)))

;; returns K(true) or KI(false)
(defmacro (le-lt xs ys)
  (vm-bits (lambda (g xs ys borrow)
	     (g (cdr xs) (cdr ys) (borrow-table (car xs) (car ys) borrow)))
	   (lambda (xs ys b) b)
	   xs ys KI))

(defrecmacro (->le-inc)
  (icons (lambda (tl) ((icons KI) (tl ->le-inc)))
	 (icons K)))

(defmacro (le-inc xs)
  (xs ->le-inc))

(defrecmacro (->le-dec)
  (icons (icons KI)
	 (lambda (tl) ((icons K) (tl ->le-dec)))))

(defmacro (le-dec xs)
  (xs ->le-dec))

;; for debug
(defrecmacro (le->number xs)
  (if (pair? xs)
      (((car xs) succ I)
       (dbl (le->number (cdr xs))))
      c0))

(defmacro (print-le n)
  (print-digit (le->number n) I #\newline I))

;; memory ------------------------------------------------------------

(defrecmacro (zero-memory bits)
  (if (cons1? bits)
      ((lambda (a f) (f a a))
       (zero-memory (1-of-1 bits)))
      le-0))

(defrecmacro (initialize-memory-rec bits lst)
  (if (pair? lst)
      (if (cons1? bits)
	  ((initialize-memory-rec (1-of-1 bits) lst)
	   (lambda (left lst2)
	     ((initialize-memory-rec (1-of-1 bits) lst2)
	      (lambda (right lst3)
		(cons (cons right left) lst3)))))
	  lst)
      (cons (zero-memory bits) nil)))

(defmacro (initialize-memory data)
  (car (initialize-memory-rec (to-cons1 vm-bits) data)))

(defmacro (load-le mem)
  (vm-bits (lambda (f a) (f (cdr a) (car a)))
	   (K mem)))

(defmacro (store-step f g addr)
  (f ((car addr)
      (lambda (m) (cons (g (car m)) (cdr m)))
      (lambda (m) (cons (car m) (g (cdr m)))))
     (cdr addr)))

(defmacro (store-le mem addr val)
  (vm-bits store-step
	   K       ; (lambda (g addr) g)
	   (K val) ; (lambda (oldval) val)
	   addr
	   mem))

; Runner -------------------------------------------------------------

(defmacro run
  (lambda (code initial-vm)
    (let rec ((vm initial-vm))
      (rec ((load-le code (vm-pc vm)) (inc-pc vm le-inc))))))

; Runtime library ----------------------------------------------------

(defmacro (mem-load vm)
  (load-le (vm-memory vm)))

(defmacro (mem-store vm addr val)
  (set-memory vm (store-le (vm-memory vm) addr val)))

;; TODO: make this 8-bit clean
(define (putc-func bits)
  (let* ((d (length bits))
	 (hd (string->symbol (string-append "hd" (number->string d))))
	 (tl (string->symbol (string-append "tl" (number->string d)))))
    (if (= (length bits) 6) ; Only supports characters up to 127
	`(lambda (,hd _)
	   (,hd ,(integer->char (list->integer (cons #t bits)))
		,(integer->char (list->integer (cons #f bits)))))
	`(lambda (,hd ,tl)
	   (,tl (,hd ,(putc-func (cons #t bits))
		     ,(putc-func (cons #f bits))))))))

(add-unl-macro! 'putc '(n) `(n ,(putc-func '()) I))

(add-unl-macro! 'getc '()
  `(lambda (cont)
     (@ I
	,(fold-right
	  (lambda (i rest)
	    `(((? ,(integer->char i)) I cont ,(le-number i))
	      ,rest))
	  '(cont le-0)
	  (filter
	   (lambda (i)
	     (let ((c (integer->char i)))
	       (or (char-whitespace? c) (not (eq? (char-general-category c) 'Cc)))))
	   (iota 128))))))

(defmacro all-libs (cons* le-add le-sub le-eq le-lt mem-load mem-store putc getc))
(defmacro core-libs (cons* le-add le-sub le-eq le-lt mem-load mem-store ""))

(defmacro test-code (list (lambda (vm) (mem-store vm be-1 le-1))
			  (lambda (vm) (K vm (print-le (lib-load vm vm be-1))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (exit I))))

(defmacro (initial-vm data lib)
  (cons initial-regs
	(cons (initialize-memory data)
	      lib)))

(defmacro (main lib)
  (lambda (code data)
    (run (initialize-memory code) (initial-vm data lib))))

(define (main args)
  (if (equal? (cdr args) '("--generate-core"))
      (begin
	(print-as-unl '(main core-libs))
	(exit)))
  (let ((parsed (parse)))
    (print "``")
    (print "# VM core")
    (print-as-unl '(main all-libs))
    (newline)
    (generate (car parsed) (cadr parsed))
    0))
