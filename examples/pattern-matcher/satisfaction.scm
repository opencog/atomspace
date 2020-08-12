;
; satisfaction.scm -- Determining satisfiability of a query.
;
; SatisfactionLink usage example.
;
; Using the SatisfactionLink is fairly straightforward; what this
; example shows is how to obtain the variable grounding that caused
; the satisfaction to be fulfilled.
;

(use-modules (opencog) (opencog exec))

; Define a very simple satisfaction link.
(define satlink
	(Satisfaction
		(Evaluation
			(Predicate "foobar")
			(List
				(Concept "funny")
				(Variable "$x")))))

; Create something that will satisfy the above.
(Evaluation
	(Predicate "foobar")
	(List
		(Concept "funny")
		(Concept "thing")))

; Actually run it - this should return TrueTV i.e. `(stv 1 1)`
; because the SatisfactionLink is satisfiable.
(cog-execute! satlink)

; Print a list of all the keys attached to the SatisfactionLink.
(cog-keys satlink)

; The one and only key printed above should be the below.
(define pgk (Predicate "*-PatternGroundingsKey-*"))

; Get the value associated with this key.
; Ah Ha!  It should print `(Concept "thing")` which is the
; value that grounded the `(Variable "$x")`.
;
; This value is cached indefinitely - until the next time that
; the SatisfactionLink is evaluated. If it evaluates to false,
; the old cached value is NOT cleared!
(cog-value satlink pgk)
