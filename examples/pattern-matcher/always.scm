;
; always.scm -- Demo illustrating AlwaysLink ("for-all")
;
; The most basic kinds of pattern searches are to see if some
; particular pattern can be found. These are "exists" searches;
; does the pattern exist?  Sometimes one wants to have "for-all"
; searches: does **every** instance of the pattern **always**
; satisfy some predicate or term? Such searches can be implemented
; with the AlwaysLink.
;
; In this example, there are four baskets holding balls. Some
; hold balls of several different colors, some hold only balls
; that are all of the same color. The example sets up these baskets,
; and then issues a query to find the basket that holds only red
; balls, and no others. A fancier query finds those baskets that
; have balls all of the same color (whatever color that might be).
;
(use-modules (opencog) (opencog exec))

; ---------------------------------------------------------
; Populate the AtomSpace with baskets holding balls.

; Three baskets holding balls
(Inheritance (Concept "reds basket")        (Concept "basket"))
(Inheritance (Concept "another red basket") (Concept "basket"))
(Inheritance (Concept "reds&greens basket") (Concept "basket"))
(Inheritance (Concept "yellows basket")     (Concept "basket"))

; Balls placed into baskets
(Member (Concept "red ball")      (Concept "reds basket"))
(Member (Concept "red ball too")  (Concept "reds basket"))
(Member (Concept "red ball also") (Concept "reds basket"))

(Member (Concept "a red ball")    (Concept "another red basket"))
(Member (Concept "b red ball")    (Concept "another red basket"))
(Member (Concept "red ball also") (Concept "another red basket"))

(Member (Concept "red ball")      (Concept "reds&greens basket"))
(Member (Concept "red ball too")  (Concept "reds&greens basket"))
(Member (Concept "green ball")    (Concept "reds&greens basket"))

(Member (Concept "yellow ball")   (Concept "yellows basket"))
(Member (Concept "ochre ball")    (Concept "yellows basket"))

; Predicate that tests the colors of the balls
(Evaluation (Predicate "is red") (Concept "red ball"))
(Evaluation (Predicate "is red") (Concept "red ball too"))
(Evaluation (Predicate "is red") (Concept "red ball also"))
(Evaluation (Predicate "is red") (Concept "a red ball"))
(Evaluation (Predicate "is red") (Concept "b red ball"))

(Evaluation (Predicate "is green")  (Concept "green ball"))

(Evaluation (Predicate "is yellow") (Concept "yellow ball"))
(Evaluation (Predicate "is yellow") (Concept "ochre ball"))

; ---------------------------------------------------------
; The query below will search for those baskets that have
; only red balls in them.
(define get-baskets-with-only-red-balls
	(Bind
		(VariableList
			(TypedVariable (Variable "basket") (Type 'ConceptNode))
			(TypedVariable (Variable "ball")   (Type 'ConceptNode))
		)
		(And
			; Look at every basket ...
			(Inheritance (Variable "basket") (Concept "basket"))

			; Look at the balls in the basket ...
			(Member (Variable "ball") (Variable "basket"))

			; Always means that *every* ball in the basket MUST
			; be red! Any single failure to satisfy this invalidates
			; the entire search.
			(Always (Evaluation (Predicate "is red") (Variable "ball")))
		)

		; Report the basket which has only red balls in it.
		(Variable "basket"))
)

; Now run the query above ... it should find two baskets, the
; "reds basket" and "another red basket".
(cog-execute! get-baskets-with-only-red-balls)

; ---------------------------------------------------------
; Now lets generalize. Look for those baskets which all have
; the same color, no matter what that might be. This time, the
; color comparison is done pair-wise. Every possible pair of
; balls in the basket are compared, and if they always match,
; then the basket is reported.

(define baskets-with-same-color
	(Bind
		(VariableList
			(TypedVariable (Variable "basket")      (Type 'ConceptNode))
			(TypedVariable (Variable "some ball")   (Type 'ConceptNode))
			(TypedVariable (Variable "other ball")  (Type 'ConceptNode))
			(TypedVariable (Variable "some color")  (Type 'PredicateNode))
			(TypedVariable (Variable "other color") (Type 'PredicateNode))
		)
		(And
			; We are only interested in baskets.
			(Inheritance (Variable "basket")       (Concept "basket"))

			; Pick out two balls from the same basket.
			(Member (Variable "some ball")         (Variable "basket"))
			(Member (Variable "other ball")        (Variable "basket"))
			(Evaluation (Variable "some color")    (Variable "some ball"))
			(Evaluation (Variable "other color")   (Variable "other ball"))

			; Are those two balls of the same color?
			(Always (Equal (Variable "some color") (Variable "other color")))
		)
		(Variable "basket"))
)

; Now run the query above ... it should find three baskets, the
; two red ones, and also the yellow one.
(cog-execute! baskets-with-same-color)

; ---------------------------------------------------------
; One can achieve some of the same effect by cascading multiple
; queries. In the below, a nested inner query performs a search
; for baskets with balls that are not red, and then rejects such
; baskets.

(define not-baskets-with-not-red
	; Return those things that ....
	(Get (Variable "basket")
		(And
			; ... things that are baskets ...
			(Inheritance (Variable "basket") (Concept "basket"))
			; ... but are not ...
			(NotLink
				; SatisfactionLink returns true/false: true when
				; the clauses can be satisfied, else, false.
				; In this case, "true" means there exists a ball that ...
				(SatisfactionLink (Variable "ball")
					(And
						; ... a ball that is in a basket, and ...
						(Member (Variable "ball") (Variable "basket"))
						; ... the ball is not red. That is, the clause
						; below cannot be found in the AtomSpace.
						(Absent
							(Evaluation (Predicate "is red") (Variable "ball")))
						; So, the SatisfactionLink evaluates to "true" if
						; if the basket contains some ball (any ball) that
						; is not red.
					))))
		; ... NotLink: true, if the basket didn't have any balls
		; that were not red. That is, only baskets with red balls
		; are found.
	))

; Run the above query.
(cog-execute! not-baskets-with-not-red)

; This query is less efficient than using the AlwaysLink, mostly
; because it consists of two nested queries, and the inner query
; needs to be setup before it can be run. The setup and the distinct
; search it performs will in general use up more CPU time than the
; single query leveraging the AlwaysLink.
; ---------------------------------------------------------
