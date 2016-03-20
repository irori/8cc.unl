#!/usr/bin/gosh
(require "./unlc.scm")
(require "./lib.scm")
(require "./compile.scm")
(use srfi-60)

(defmacro (bit-not x) (x KI K))
(defmacro (bit-or x y) (x K y))
(defmacro (bit-and x y) (x y KI))
(defmacro (bit-xor x y) (x (bit-not y) y))
(defmacro (bit-xor3 x y z) (bit-xor (bit-xor x y) z))

(defmacro LT (lambda (x y z) x))
(defmacro EQ (lambda (x y z) y))
(defmacro GT (lambda (x y z) z))

(defmacro le-0 (c16 (cons KI) nil))
(defmacro be-0 (c16 (cons KI) nil))
(defmacro le-1 (cons K (c15 (cons KI) nil)))
(defmacro be-1 (list KI KI KI KI KI KI KI KI KI KI KI KI KI KI KI K))

(defmacro c65536 (pow c4 c8))

;; little endian binary number ---------------------------------------

(defrecmacro (le-add-rec xs ys carry)
  (if (pair? xs)
      (cons (bit-xor3 (car xs) (car ys) carry)
	    (le-add-rec (cdr xs) (cdr ys)
			((carry bit-or bit-and) (car xs) (car ys))))
      nil))

(defmacro (le-add xs ys) (le-add-rec xs ys KI))

(defrecmacro (le-sub-rec xs ys borrow)
  (if (pair? xs)
      (cons (bit-xor3 (car xs) (car ys) borrow)
	    (le-sub-rec (cdr xs) (cdr ys)
			(((car xs) bit-and bit-or) (car ys) borrow)))
      nil))

(defmacro (le-sub xs ys) (le-sub-rec xs ys KI))

; returns K(true) or KI(false)
(defrecmacro (le-eq xs ys)
  (if (pair? xs)
      (if ((bit-xor (car xs) (car ys)) I V)
	  KI
	  (le-eq (cdr xs) (cdr ys)))
      K))

; returns K(true) or KI(false)
(defrecmacro (le-lt-rec xs ys borrow)
  (if (pair? xs)
      (le-lt-rec (cdr xs) (cdr ys)
		 (((car xs) bit-and bit-or) (car ys) borrow))
      borrow))

(defmacro (le-lt xs ys) (le-lt-rec xs ys KI))

(defrecmacro (le-inc xs)
  (if (pair? xs)
      (if ((car xs) I V)
	  (cons KI (le-inc (cdr xs)))
	  (cons K (cdr xs)))
      nil))

(defrecmacro (le-dec xs)
  (if (pair? xs)
      (if ((car xs) I V)
	  (cons KI (cdr xs))
	  (cons K (le-dec (cdr xs))))
      nil))

(defrecmacro (le->number xs)
  (if (pair? xs)
      (((car xs) succ I)
       (dbl (le->number (cdr xs))))
      c0))

;; big endian binary number ------------------------------------------

(defrecmacro (be-add-rec xs ys)
  (if (pair? xs)
      ((be-add-rec (cdr xs) (cdr ys))
       (lambda (carry r)
	 (cons ((carry bit-or bit-and) (car xs) (car ys))
	       (cons (bit-xor3 (car xs) (car ys) carry)
		     r))))
      (cons KI nil)))

(defmacro (be-add xs ys)
  (cdr (be-add-rec xs ys)))

(defrecmacro (be-inc-rec xs)
  (if (pair? xs)
      ((be-inc-rec (cdr xs))
       (lambda (carry r)
	 (cons (bit-and (car xs) carry)
	       (cons (bit-xor (car xs) carry)
		     r))))
      (cons K nil)))

(defmacro (be-inc xs)
  (cdr (be-inc-rec xs)))

(defrecmacro (be-sub-rec xs ys)
  (if (pair? xs)
      ((be-sub-rec (cdr xs) (cdr ys))
       (lambda (borrow r)
	 (cons (((car xs) bit-and bit-or) (car ys) borrow)
	       (cons (bit-xor3 (car xs) (car ys) borrow)
		     r))))
      (cons KI nil)))

(defmacro (be-sub xs ys)
  (cdr (be-sub-rec xs ys)))

(defrecmacro (be-cmp xs ys)
  (if (pair? xs)
      (if ((car xs) ((car ys) I V) ((car ys) V I))
	  (be-cmp (cdr xs) (cdr ys))
	  ((car xs) GT LT))
      EQ))

(defrecmacro (be->number-rec acc xs)
  (if (pair? xs)
      (be->number-rec (((car xs) succ I) (dbl acc))
		      (cdr xs))
      acc))

(defmacro be->number (be->number-rec c0))

;; memory ------------------------------------------------------------

(defrecmacro (zero-memory bits)
  (if (cons1? bits)
      (icons (zero-memory (1-of-1 bits))
	     (zero-memory (1-of-1 bits)))
      (c16 (cons KI) nil)))

(defrecmacro (initialize-memory bits lst)
  (if (pair? lst)
      (if (cons1? bits)
	  ((initialize-memory (1-of-1 bits) lst)
	   (lambda (left lst2)
	     ((initialize-memory (1-of-1 bits) lst2)
	      (lambda (right lst3)
		(cons (cons left right) lst3)))))
	  lst)
      (cons (zero-memory bits) nil)))

(add-unl-macro! 'data-section () (data-section))

(defmacro initial-memory (car (initialize-memory (to-cons1 c16) data-section)))

(defrecmacro (le-loader addr)
  (if (pair? addr)
      (compose ((car addr) cdr car) (le-loader (cdr addr)))
      I))

(defmacro (load-le mem addr)
  ((le-loader addr) mem))

(defrecmacro (store-le-rec f addr)
  (if (pair? addr)
      (store-le-rec
       ((car addr)
	(lambda (m) (cons (car m) (f (cdr m))))
	(lambda (m) (cons (f (car m)) (cdr m))))
       (cdr addr))
      f))

(defmacro (store-le mem addr val)
  ((store-le-rec (lambda (oldval) val) addr) mem))

(defrecmacro (load-be mem addr)
  (if (pair? addr)
      (load-be (((car addr) cdr car) mem) (cdr addr))
      mem))

(defmacro (store-be mem addr val)
  (let rec ((m mem) (adr addr))
    (if (pair? adr)
	(let ((r (rec (((car adr) cdr car) m) (cdr adr))))
	  ((car adr)
	   (cons (car m) r)
	   (cons r (cdr m))))
	val)))

(defmacro memory-write-test
  (c65536
   (lambda (s)
     (s (lambda (mem addr)
	  (cons (store-le mem addr addr) (le-inc addr)))))
   (cons initial-memory le-0)))

(defmacro memory-test
  (let ((mem memory-write-test))
    (c65536
     (lambda (addr) (K (le-inc addr) (load-le mem addr)))
     le-0)))

; VM state -----------------------------------------------------------

(defmacro initial-regs
  (list le-0 le-0 le-0 le-0 le-0 le-0 le-0)) ; PC, A, B, C, D, BP, SP

(defmacro (regs-pc regs) (car regs))
(defmacro (regs-set-pc regs val)
  (cons val (cdr regs)))

(defmacro (vm-regs vm) (car vm))
(defmacro (set-regs vm regs)
  (cons regs (cdr vm)))
(defmacro (set-reg vm regno val)
  (set-regs vm (update-nth (K val) regno (vm-regs vm))))
(defmacro (vm-memory vm) (cadr vm))
(defmacro (set-memory vm mem) (cons (car vm)
				    (cons mem (cddr vm))))
(defmacro (vm-code vm) (car (cddr vm)))

(defmacro (vm-pc vm)
  (regs-pc (vm-regs vm)))
(defmacro (set-pc vm pc)
  (set-regs vm (regs-set-pc (vm-regs vm) pc)))

(defrecmacro (run vm)
  (let* ((pc (vm-pc vm))
	 (code (load-le (vm-code vm) pc)))
	 ;(code (nth (le->number pc) (vm-code vm))))
    (run (code (set-pc vm (le-inc pc))))))


; Runner -------------------------------------------------------------

(defmacro (mem-load vm addr)
  (load-le (vm-memory vm) addr))

(defmacro (mem-store vm addr val)
  (set-memory vm (store-le (vm-memory vm) addr val)))

;; TODO: make putc 8-bit clean
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
	(,@(map (lambda (i)
		  `((? ,(integer->char i)) I cont ,(le-number2 i)))
		(filter
		 (lambda (i)
		   (let ((c (integer->char i)))
		     (or (char-whitespace? c) (not (eq? (char-general-category c) 'Cc)))))
		 (iota 128)))
	 (cont le-0)))))

(defmacro (print-le n)
  (print-digit (le->number n) I #\newline I))

(defmacro lib (list le-add le-sub le-eq le-lt mem-load mem-store putc getc))
(defmacro lib-add (nth c3))
(defmacro lib-sub (nth c4))
(defmacro lib-eq (nth c5))
(defmacro lib-lt (nth c6))
(defmacro lib-load (nth c7))
(defmacro lib-store (nth c8))
(defmacro lib-putc (nth c9))
(defmacro lib-getc (nth c10))

(defmacro test-code (list (lambda (vm) (mem-store vm be-1 le-1))
			  (lambda (vm) (K vm (print-le (lib-load vm vm be-1))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (exit I))))

(add-unl-macro! 'code '() (compile-code))

(defmacro code-memory (car (initialize-memory (to-cons1 c16) code)))

(defmacro initial-vm
  (cons initial-regs
	(cons initial-memory
	      (cons code-memory
		    lib))))


(defmacro test
  (#\* (call/cc getc)
   I #\newline I))


(define (main args)
  (print-as-unl '(run initial-vm))
  0)

