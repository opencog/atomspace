;
; virtual.scm --  Demo the internal workings of virtual links.
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
; be evaluated. An example of this is the following query:
;
; find cities $m with population $x and countries $c with population $y
; such that $y < $x (the city is larger than the country)
;
; In the above, one graph component is the set of assertions about
; cities and their population. The other graph component is the set of
; assertions about countries and their populations.  These two sets
; must then be pair-wise compared.
;
; There are similar queries which do not split into disconnected
; components. An example of this is the query:
;
; find cities $m with population $x and phone-numbers $y such that $x < $y
;
; In this case, the population and the phone-numbers live together in
; the same connected graph; one merely has to find all of these graphs,
; and then reject some of them with the comparison.
;
; The code below illustrates both of these cases, the second one first.


(use-modules (opencog))
(use-modules (opencog exec))

; Define three cities
(Evaluation (Predicate "phone") (List (Concept "Paris") (Number 10)))
(Evaluation (Predicate "pop") (List (Concept "Paris") (Number 5)))
(Evaluation (Predicate "org") (List (Concept "Paris") (Concept "city")))

(Evaluation (Predicate "phone") (List (Concept "Berlin") (Number 10)))
(Evaluation (Predicate "pop") (List (Concept "Berlin") (Number 15)))
(Evaluation (Predicate "org") (List (Concept "Berlin") (Concept "city")))

(Evaluation (Predicate "phone") (List (Concept "Hong Kong") (Number 10)))
(Evaluation (Predicate "pop") (List (Concept "Hong Kong") (Number 20)))
(Evaluation (Predicate "org") (List (Concept "Hong Kong") (Concept "city")))


; Define two countries
(Evaluation (Predicate "pop") (List (Concept "Barbados") (Number 7)))
(Evaluation (Predicate "org") (List (Concept "Barbados") (Concept "country")))

(Evaluation (Predicate "pop") (List (Concept "Japan") (Number 30)))
(Evaluation (Predicate "org") (List (Concept "Japan") (Concept "country")))

; Define a greater-than function. Aside from returning true/false if
; the greater-than relation holds or not, it also counts the number of
; times that it was invoked. This allows us to monitor how many
; comparisons are made.
(define cnt 0)
(define (cmp nx ny)
	(define x (string->number (cog-name nx)))
	(define y (string->number (cog-name ny)))
	(set! cnt (+ cnt 1)) 
	(if (> x y) (stv 1 1) (stv 0 1)))

; A query that looks for cities where there are more phones than people.
(define phone-inversion
(Get (And
	(Evaluation (Predicate "phone")
		 (List (Variable "$city") (Variable "#phone")))
	(Evaluation (Predicate "pop")
		 (List (Variable "$city") (Variable "#pop")))
	;;(GreaterThan
	;;	(Variable "#phone") (Variable "#pop"))
	(Evaluation
		(GroundedPredicate "scm: cmp")
		(List (Variable "#phone") (Variable "#pop")))
	))
)

; Perform the query.
(cog-execute! phone-inversion)

; Display the count.
cnt
; The above should display "3", because there are 3 cities to be
; examined. It should NOT display 9 = 3 x 3 because we should not
; be comparing city-pairs first, and checking city-equality second.


; A query that looks for cities that are larger than countries.
(define pop-inversion
(Get (And
	(Evaluation (Predicate "org")
		 (List (Variable "$city") (Concept "city")))
	(Evaluation (Predicate "pop")
		 (List (Variable "$city") (Variable "#city-pop")))
	(Evaluation (Predicate "org")
		 (List (Variable "$state") (Concept "country")))
	(Evaluation (Predicate "pop")
		 (List (Variable "$state") (Variable "#state-pop")))
	(EvaluationLink
		(GroundedPredicateNode "scm: cmp")
		(List (Variable "#city-pop") (Variable "#state-pop")))
	))
)

; Reset the count and perform the query.
(set! cnt 0)
(cog-execute! pop-inversion)

; Display the count.
cnt
; The above should display 6 = 2 x 3 as there are two countries to be
; compared to three cities.  The comparisons must be down pair-wise,
; resulting in a combinatoric explosion in the number of comparisons to
; be performed.
