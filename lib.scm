;; Library macros & syntaxes

(define-module lib
  (use unlambda.compiler)
  (use unlambda.prelude)
  (use srfi-1)
  )
(select-module lib)

(defsyntax (clist . es)
  (fold-right (lambda (hd tl) `(cons ,hd ,tl))
	      'nil
	      es))

(defmacro (replace-nth val n)
  (n
   (lambda (g lst)
     (lst (lambda (hd tl)
	    (S (S I (K hd)) (K (g tl))))))  ; (cons hd (g tl))
   ((lambda (_v)
      (lambda (_lst)
	(_lst (lambda (_hd _tl)
		((snoc _tl) _v)))))
    val)))
