;
; glob-nest.scm
;
; Test nested glob expressions.
;

; The use of the MemberLink here messes it up.
; Earlier code incorrectly finds only one instance.
(define (body WORD)
	(List
		(Variable "$point")
		(OrderedLink
			(Glob "$begin")
			(Member (Variable "$set") WORD)
			(Glob "$end"))))

(define (locate WORD)
	(Bind (VariableList
		(TypedVariable (Variable "$point") (Type 'ConceptNode))
		(TypedVariable (Variable "$set") (Type 'ConceptNode))
		(TypedVariable (Glob "$begin") (Interval (Number 0) (Number -1)))
		(TypedVariable (Glob "$end") (Interval (Number 0) (Number -1))))
		(body WORD)(body WORD)))

(define get-five (locate (Concept "special")))

; Like above, but with more globbiness....
(define (gbody WORD)
	(List
		(Variable "$point")
		(OrderedLink
			(Glob "$begin")
			(Member WORD (Glob "$set"))
			(Glob "$end"))))

(define (glocate WORD)
	(Bind (VariableList
		(TypedVariable (Variable "$point") (Type 'ConceptNode))
		(TypedVariable (Glob "$set")
			(TypeSet (Type 'ConceptNode)
				(IntervalLink (Number 1) (Number 1))))
		(TypedVariable (Glob "$begin") (Interval (Number 0) (Number -1)))
		(TypedVariable (Glob "$end") (Interval (Number 0) (Number -1))))
		(gbody WORD)(gbody WORD)))

(define get-four (glocate (Concept "stuff")))

(define one
(List
	(Concept "first")
	(OrderedLink
		(Member (Concept "stuff") (Concept "special"))
		(Concept "A")
		(Concept "B")
		(Concept "C")
		(Concept "D"))))

(define two
(List
	(Concept "second")
	(OrderedLink
		(Concept "A")
		(Member (Concept "things") (Concept "special"))
		(Concept "B")
		(Concept "C")
		(Concept "D"))))

(define three
(List
	(Concept "third")
	(OrderedLink
		(Concept "A")
		(Concept "B")
		(Member (Concept "stuff") (Concept "special"))
		(Concept "C")
		(Concept "D"))))

(define four
(List
	(Concept "fourth")
	(OrderedLink
		(Concept "A")
		(Concept "B")
		(Concept "C")
		(Member (Concept "stuff") (Concept "special"))
		(Concept "D"))))

(define five
(List
	(Concept "fifth")
	(OrderedLink
		(Concept "A")
		(Concept "B")
		(Concept "C")
		(Concept "D")
		(Member (Concept "stuff") (Concept "special")))))

*unspecified*
