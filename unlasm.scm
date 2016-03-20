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

;; little endian binary number ---------------------------------------

(defmacro le-0 (vm-bits (cons KI) nil))
(defmacro le-1 (cons K (vm-bits-1 (cons KI) nil)))

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

;; returns K(true) or KI(false)
(defrecmacro (le-eq xs ys)
  (if (pair? xs)
      (if ((bit-xor (car xs) (car ys)) I V)
	  KI
	  (le-eq (cdr xs) (cdr ys)))
      K))

;; returns K(true) or KI(false)
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
      (icons (zero-memory (1-of-1 bits))
	     (zero-memory (1-of-1 bits)))
      le-0))

(defrecmacro (initialize-memory-rec bits lst)
  (if (pair? lst)
      (if (cons1? bits)
	  ((initialize-memory-rec (1-of-1 bits) lst)
	   (lambda (left lst2)
	     ((initialize-memory-rec (1-of-1 bits) lst2)
	      (lambda (right lst3)
		(cons (cons left right) lst3)))))
	  lst)
      (cons (zero-memory bits) nil)))

(defmacro (initialize-memory data)
  (car (initialize-memory-rec (to-cons1 vm-bits) data)))

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

; VM state -----------------------------------------------------------

; Register order: PC, A, B, C, D, BP, SP
(defmacro num-regs c7)
(defmacro initial-regs
  (num-regs (cons le-0) nil))

(defmacro (replace-car lst x) (cons x (cdr lst)))

(defmacro (vm-regs vm) (car vm))
(defmacro (set-reg vm regno val)
  (replace-car vm (update-nth (K val) regno (vm-regs vm))))
(defmacro (vm-memory vm) (cadr vm))
(defmacro (set-memory vm mem) (cons (car vm)
				    (cons mem (cddr vm))))
(defmacro (vm-code vm) (car (cddr vm)))

(defmacro (vm-pc vm)
  (car (vm-regs vm)))
(defmacro (set-pc vm pc)
  (replace-car vm (replace-car (vm-regs vm) pc)))

(defrecmacro (run vm)
  (let* ((pc (vm-pc vm))
	 (code (load-le (vm-code vm) pc)))
    (run (code (set-pc vm (le-inc pc))))))


; Runner -------------------------------------------------------------

(defmacro (mem-load vm addr)
  (load-le (vm-memory vm) addr))

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
	(,@(map (lambda (i)
		  `((? ,(integer->char i)) I cont ,(le-number2 i)))
		(filter
		 (lambda (i)
		   (let ((c (integer->char i)))
		     (or (char-whitespace? c) (not (eq? (char-general-category c) 'Cc)))))
		 (iota 128)))
	 (cont le-0)))))

(defmacro lib (list le-inc le-dec le-add le-sub le-eq le-lt mem-load mem-store putc getc))
(defmacro lib-inc (nth c3))
(defmacro lib-dec (nth c4))
(defmacro lib-add (nth c5))
(defmacro lib-sub (nth c6))
(defmacro lib-eq (nth c7))
(defmacro lib-lt (nth c8))
(defmacro lib-load (nth c9))
(defmacro lib-store (nth c10))
(defmacro lib-putc (nth c11))
(defmacro lib-getc (nth c12))

(defmacro test-code (list (lambda (vm) (mem-store vm be-1 le-1))
			  (lambda (vm) (K vm (print-le (lib-load vm vm be-1))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (K vm (print-le (vm-pc vm))))
			  (lambda (vm) (exit I))))

(add-unl-macro! 'initial-data () (initial-data))
(add-unl-macro! 'code '() (compile-code))

(defmacro initial-vm
  (cons initial-regs
	(cons (initialize-memory initial-data)
	      (cons (initialize-memory code)
		    lib))))

(define (main args)
  (print-as-unl '(run initial-vm))
  0)
