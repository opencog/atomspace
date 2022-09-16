;
; metta-lisp.scm
;
; Experimental demo of a quasi-MeTTa-compatible Lisp-like language.
;
(use-modules (opencog) (opencog exec))

; Define a factorial function
(LispAst "(= (fact $x) (if (< $x 2) 1 (* $x (fact (- $x 1)))))")

; Run it.
(cog-execute! (LispAst "(fact 5)"))

; Define a simple named numeric value
(LispAst "(= foo 6)")

; Run it.
(cog-execute! (LispAst "(fact foo)"))
