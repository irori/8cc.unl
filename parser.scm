(define-module parser
  (use srfi-13)
  (use gauche.sequence)
  (export vm-bits parse)
  )
(select-module parser)

(define vm-bits
  (if (sys-getenv "BFS24") 24 16))

(define UINT_MAX (- (ash 1 vm-bits) 1))
(define INT_MIN (- (ash 1 (- vm-bits 1))))

(define (remove-comment line)
  (if (eq? (string-ref line 0) #\#)
      ""
      (string-trim (regexp-replace #/\s# .*/ line ""))))

(define jmp-ops
  '((jmp . 1)
    (jeq . 3)
    (jne . 3)
    (jlt . 3)
    (jgt . 3)
    (jle . 3)
    (jge . 3)))

(define ops
  (append
   '((mov . 2)
     (add . 2)
     (sub . 2)
     (load . 2)
     (store . 2)
     (eq . 2)
     (ne . 2)
     (lt . 2)
     (gt . 2)
     (le . 2)
     (ge . 2)
     (putc . 1)
     (getc . 1)
     (exit . 0)
     (dump . 0))
   jmp-ops))

(define (parse)
  (define *code* '(((jmp ("main") -1))))
  (define (cur-code-addr) (length *code*))
  (define *data* '(0))
  (define (cur-data-addr) (+ (length *data*) 256))
  (define (add-data byte) (push! *data* byte))
  (define labels (make-hash-table 'string=?))
  (define labeled-pcs (make-hash-table))

  (hash-table-put! labels "main" 1)
  (hash-table-put! labels "_edata" 256)
  (hash-table-put! labeled-pcs 1 #t)

  (let ((in-data #f)
	(prev-op #f)
	(lineno 0))
    (port-for-each
     (lambda (line)
       (inc! lineno)

       (rxmatch-case (remove-comment line)

         (#/^\.set\s+(\w+)\s*,\s*(.+)/ (#f name val)
	   (hash-table-put! labels name
			    (or (hash-table-get labels val #f) (string->number val))))

	 (#/^\.text$/ (#f)
	   (set! in-data #f))

	 (#/^\.data(\s*(-?\d+))?/ (#f #f n)
	   (set! in-data #t)
	   (if n
	       (add-data (+ (cur-data-addr) (string->number n)))))

	 (#/^(\.?\w+):$/ (#f label)
	   (if (and (hash-table-exists? labels label) (not (string=? label "main")))
	       (error "multiple label definition (" label ") at line " lineno))
	   (if in-data
	       (hash-table-put! labels label (cur-data-addr))
	       (begin
		 (hash-table-put! labeled-pcs (cur-code-addr) #t)
		 (hash-table-put! labels label (cur-code-addr)))))

	 (test (lambda (line) (and in-data line)) =>
	   (lambda (line)
	     (rxmatch-case line

		(#/^\.long (-?\d+)/ (#f n)
		  (add-data (logand UINT_MAX (string->number n))))

		(#/^\.long (\.?\w+)/ (#f label)
		  (if (not (hash-table-exists? labels label))
		      (error "undefined label " label " at line " lineno))
		  (add-data (hash-table-get labels label)))

		(#/^\.lcomm (\w+), (\d+)/ (#f name size)
		  (hash-table-put! labels name (cur-data-addr))
		  (dotimes ((string->number size))
			   (add-data 0)))

		(#/^\.(ascii|string) (".*")/ (#f cmd str) ;|)
		  (let ((str (call-with-input-string str read)))
		    (dotimes (i (string-size str))
			     (add-data (string-byte-ref str i))))
		  (if (string=? cmd "string")
		      (add-data 0)))

		(else (error "unknown data at line " lineno)))))

	 (else =>
	  (lambda (line)
	    (let* ((op-args (string-split line #/\s+/ 1))
		   (op (string->symbol (car op-args)))
		   (args (if (null? (cdr op-args))
			     '()
			     (string-split (cadr op-args) #/\s*,\s*/)))
		   (argnum (assq op ops)))
	      (if (not argnum)
		  (error "invalid op (" op ") at line " lineno))
	      (if (not (= (cdr argnum) (length args)))
		  (error "invalid number of args for " op " at line " lineno))

	      (let ((args (map-with-index (lambda (i arg) (parse-arg arg op i lineno)) args)))
		(if (and (= (cdr argnum) 2) (not (symbol? (car args))))
		    (error "invalid first arg for " op " at line " lineno))

		(if (or (hash-table-exists? labeled-pcs (cur-code-addr))
			(memq prev-op '(load store putc getc exit))
			(assq prev-op jmp-ops))
		    (push! *code* (list (list op args lineno)))
		    (set-car! *code* (cons (list op args lineno) (car *code*))))

		(set! prev-op op)))))
	 ))
     read-line))

  (set! *data* (reverse! *data*))
  (set-car! *data* (cur-data-addr))

  (set!
   *code*
   (map-with-index
    (lambda (i opa)
      (map (lambda (triple)
	     (let ((op (car triple))
		   (args (cadr triple))
		   (lineno (caddr triple)))
	       (let ((args-resolved
		      (map-with-index
		       (lambda (j a)
			 (cond ((string? a)
				(if (or (and (assq op jmp-ops) (= j 0))
					(and (eq? op 'mov) (= j 1)))
				    (or (hash-table-get labels a #f)
					(error "undefined label " a " at line " lineno))
				    (error "unexpected label (" a " in " op)))
			       ((symbol? a)
				a)
			       (else
				(if (or (< a INT_MIN) (> a UINT_MAX))
				    (error "number (" a ") out of bound at line " lineno))
				(logand UINT_MAX a))))
		       args)))
		 (append (list op) args-resolved (list lineno)))))
	   (reverse! opa)))
    (reverse! *code*)))
  (list *code* *data*))

(define (parse-arg str op i lineno)
  (or (rxmatch-case str
      (#/^0x(\d+)$/ (#f h) (string->number h 16))
      (#/^-?\d+$/ (n) (string->number n))
      (#/^(a|b|c|d|pc|bp|sp)$/i (reg) (string->symbol (string-downcase reg)))
      (#/^\.?\w+$/ (label)
	(if (or (and (assq op jmp-ops) (= i 0))
		(and (eq? op 'mov) (= i 1)))
	    label
	    #f))
      (else #f))
      (error "invalid use of " op " at line " lineno)))

;; usage: gosh -m parser parser.scm
(define (main args)
  (let ((r (parse)))
    (print (car r))
    (print (cadr r))))
