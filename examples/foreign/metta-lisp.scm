;
; metta-lisp.scm
;
; Experimental demo of a quasi-MeTTa-compatible Lisp-like language.
;
(use-modules (opencog) (opencog-exec))

(define lisp-expr
	(LispAst
		"(= (fact $x)
		 	(if (< $x 2) 1 (* $x (fact (- $x 1)))))"
	))

(cog-execute! lisp-expr)
