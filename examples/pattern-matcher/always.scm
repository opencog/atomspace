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
; In this example, there are three baskets holding balls. Some
; hold balls of several different colors, some hold only balls
; that are all of the same color. The example sets up these baskets,
; and then issues a query to find the basket that holds only red
; balls, and no others.
;
(use-modules (opencog) (opencog exec))

; Three baskets holding balls
(Inheritance (Concept "reds basket")        (Concept "basket"))
(Inheritance (Concept "reds&greens basket") (Concept "basket"))
(Inheritance (Concept "yellows basket")     (Concept "basket"))

; Balls placed into baskets
(Member (Concept "red ball")      (Concept "reds basket"))
(Member (Concept "red ball too")  (Concept "reds basket"))
(Member (Concept "red ball also") (Concept "reds basket"))
(Member (Concept "red ball")      (Concept "reds&greens basket"))
(Member (Concept "red ball too")  (Concept "reds&greens basket"))
(Member (Concept "green ball")    (Concept "reds&greens basket"))
(Member (Concept "yellow ball")   (Concept "yellows basket"))

; Predicate that tests the colors of the balls
(Evaluation (Predicate "is red") (Concept "red ball"))
(Evaluation (Predicate "is red") (Concept "red ball too"))
(Evaluation (Predicate "is red") (Concept "red ball also"))

(Evaluation (Predicate "is green")  (Concept "green ball"))
(Evaluation (Predicate "is yellow") (Concept "yellow ball"))

; The search that we will perform.
(define get-baskets-with-only-red-balls
	(Bind
		(VariableList
			(TypedVariable (Variable "basket") (Type 'ConceptNode))
			(TypedVariable (Variable "ball") (Type 'ConceptNode))
		)
		(And
			; Look at every basket ...
			(Inheritance (Variable "basket") (Concept "basket"))

			; Look at the balls in the basket ...
			(Member (Variable "ball") (Variable "basket"))

			; Always means that *every* ball in the basket MUST
			; be red! Any single failure to satisfy this invalidates
			; the entire search.
			(Always (Evaluation (Predicate "is red")  (Variable "ball")))
		)

		; Report the basket which has only red balls in it.
		(Variable "basket"))
)

(cog-execute! get-baskets-with-only-red-balls)
