;
; Demo illustrating the internal workings of virtual links.
;
; Virtual links are links that are not stored as data in the atomspace,
; but are instead evaluated "on the fly", as needed.  An example of
; such a link would be the GreaterThanLink.  It is impossible to store
; all possible number pairs, together with the truth-value of one number
; being greater than another. However, given two numbers, the truth of
; being greater than can be evaluated at that time. 
;
; Virtual links present a problem for pattern matching, in that a
; pattern, visualized as a graph, might have two disconnected graph
; components, the two components connected only by a virtual link.
; Thus, to find all possible groundings for such a graph, the groundings
; for each component must be found first, and then the virtual links must
; be evaluated.

(use-modules (opencog))
(use-modules (opencog exec))

(Evaluation (Predicate "phone") (ListLink (Concept "Paris") (Number 10)))
(Evaluation (Predicate "pop") (ListLink (Concept "Paris") (Number 5)))
(Evaluation (Predicate "org") (ListLink (Concept "Paris") (Concept "city")))

(Evaluation (Predicate "phone") (ListLink (Concept "Berlin") (Number 10)))
(Evaluation (Predicate "pop") (ListLink (Concept "Berlin") (Number 15)))
(Evaluation (Predicate "org") (ListLink (Concept "Berlin") (Concept "city")))

(Evaluation (Predicate "phone") (ListLink (Concept "Hong Kong") (Number 10)))
(Evaluation (Predicate "pop") (ListLink (Concept "Hong Kong") (Number 20)))
(Evaluation (Predicate "org") (ListLink (Concept "Hong Kong") (Concept "city")))

(Evaluation (Predicate "pop") (ListLink (Concept "Barbados") (Number 7)))
(Evaluation (Predicate "org") (ListLink (Concept "Barbados") (Concept "country")))
(Evaluation (Predicate "pop") (ListLink (Concept "Japan") (Number 30)))
(Evaluation (Predicate "org") (ListLink (Concept "Japan") (Concept "country")))

(define cnt 0)
(define (cmp nx ny)
	(define x (string->number (cog-name nx)))
	(define y (string->number (cog-name ny)))
	(set! cnt (+ cnt 1)) 
	(if (> x y) (stv 1 1) (stv 0 1)))

(define phone-inversion
(Get (And
	(Evaluation (Predicate "phone")
		 (ListLink (Variable "$city") (Variable "#phone")))
	(Evaluation (Predicate "pop")
		 (ListLink (Variable "$city") (Variable "#pop")))
	;(GreaterThan
	;	(Variable "#phone") (Variable "#pop"))
	(EvaluationLink
		(GroundedPredicateNode "scm: cmp")
		(ListLink (Variable "#phone") (Variable "#pop")))
	))
)

(cog-execute! phone-inversion)

(define pop-inversion
(Get (And
	(Evaluation (Predicate "org")
		 (ListLink (Variable "$city") (Concept "city")))
	(Evaluation (Predicate "pop")
		 (ListLink (Variable "$city") (Variable "#city-pop")))
	(Evaluation (Predicate "org")
		 (ListLink (Variable "$state") (Concept "country")))
	(Evaluation (Predicate "pop")
		 (ListLink (Variable "$state") (Variable "#state-pop")))
	(EvaluationLink
		(GroundedPredicateNode "scm: cmp")
		(ListLink (Variable "#city-pop") (Variable "#state-pop")))
	))
)

(cog-execute! pop-inversion)
