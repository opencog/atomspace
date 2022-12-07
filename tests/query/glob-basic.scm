;
; glob-basic.scm
;

(use-modules (opencog) (opencog exec))

;;; Populate the atomspace with some "sentences".
(ListLink
	(Concept "I")
	(Concept "love")
	(Concept "you"))

(ListLink
	(Concept "I")
	(Concept "really")
	(Concept "totally")
	(Concept "need")
	(Concept "you"))

(ListLink
	(Concept "I")
	(Concept "love")
	(Concept "teddy")
	(Concept "bears")
	(Concept "a")
	(Concept "lot"))

(ListLink
	(Concept "I")
	(Concept "need")
	(Concept "you")
	(Concept "now"))

(ListLink
	(Concept "they")
	(Concept "think")
	(Concept "I")
	(Concept "hate")
	(Concept "you"))

(ListLink
	(Concept "I")
	(Concept "love")
	(Number 42))

(ListLink
	(Concept "hi"))

(ListLink
	(Concept "hi")
	(Concept "Sophia"))

(ListLink
	(Concept "they")
	(Concept "really")
	(Concept "want")
	(Concept "it"))

(ListLink
	(Concept "they")
	(Concept "want")
	(Concept "it"))

(ListLink
	(Concept "the")
	(Concept "man")
	(Concept "we")
	(Concept "saw")
	(Concept "saw")
	(Concept "a")
	(Concept "saw"))

(SetLink
	(Concept "honeydew")
	(Concept "lime")
	(Concept "apple"))

;; Two different re-write rules. The first rule, immediately below,
;; says "I * you" -> "I * you too".
(define glob-you
	(BindLink
		(ListLink
			(Concept "I") (Glob "$star") (Concept "you"))
		(ListLink
			(Concept "I") (Glob "$star") (Concept "you") (Concept "too"))))

;; This one implements "I love *" -> "Hey! I love * too"
(define love-glob
	(BindLink
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))

;; Both of these patterns should "work as expected".
; (cog-execute! glob-you)
; (cog-execute! love-glob)

; -----------------------------------------------------------------
; Globs can be typed, just like variables:

(define love-type-glob
	(BindLink
		(TypedVariable (Glob "$star") (Type "NumberNode"))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))

; -----------------------------------------------------------------
; Globs can have interval restriction

(define love-interval-glob
	(BindLink
		(TypedVariable (Glob "$star") (IntervalLink (Number 0) (Number 1)))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))

; -----------------------------------------------------------------
; Globs can have both type and interval restrictions by using TypeIntersectionLink
; type == ConceptNode and interval == zero to infinity

(define love-typeset-glob
	(BindLink
		(TypedVariable (Glob "$star")
			(TypeIntersectionLink (IntervalLink (Number 0) (Number -1)) (Type "ConceptNode")))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))

; -----------------------------------------------------------------
; Glob with intersection of interval with empty interval

(define love-interval-glob-empty-intersection
	(BindLink
		(TypedVariable (Glob "$star")
			(TypeIntersectionLink
				(IntervalLink (Number 0) (Number -1))
				(IntervalLink (Number 1) (Number 0))
				(Type "ConceptNode")))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))

; -----------------------------------------------------------------
; Slightly more complicated

(define love-three-globs
	(BindLink
		(VariableList
			(TypedVariable (Glob "$x") (IntervalLink (Number 0) (Number -1)))
			(TypedVariable (Glob "$y")
				(TypeIntersectionLink (Type "ConceptNode") (IntervalLink (Number 1) (Number 1))))
			(TypedVariable (Glob "$z") (IntervalLink (Number 0) (Number -1))))
		(ListLink
			(Glob "$x")
			(Concept "I")
			(Glob "$y")
			(Concept "you")
			(Glob "$z"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Glob "$y")
			(Concept "you")
			(Concept "also"))))

; Two globs in a row
; Should match to "hi Sophia" but not "hi" as we need to ground $y
(define greet
	(BindLink
		(VariableList
			(TypedVariable (Glob "$x") (IntervalLink (Number 0) (Number -1)))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode") (IntervalLink (Number 1) (Number -1))))
			(TypedVariable (Glob "$z") (IntervalLink (Number 0) (Number -1))))
		(ListLink
			(Glob "$x")
			(Concept "hi")
			(Glob "$y")
			(Glob "$z"))
		(ListLink
			(Concept "hi")
			(Concept "I")
			(Concept "am")
			(Glob "$y"))))

; Exactly 3 atoms to be grounded
; Should match "they really want it" but not "they want it" due to the
; interval restriction
(define exact
	(Bind
		(TypedVariable (Glob "$x") (IntervalLink (Number 3) (Number 3)))
		(ListLink
			(Concept "they")
			(Glob "$x"))
		(ListLink
			(Concept "I")
			(Glob "$x")
			(Concept "too"))))

; Match as many as possible, should not stop when it gets to
; the first "saw"
(define greedy
	(Bind
		(TypedVariable (Glob "$x")
			(TypeIntersection (Type "ConceptNode")
				(IntervalLink (Number 1) (Number -1))))
		(ListLink
			(Glob "$x")
			(Concept "saw"))
		(ListLink
			(Glob "$x")
			(Concept "cat")
			(Concept "too"))))

; Match in any order
(define unorder
	(Bind
		(TypedVariable (Glob "$x") (Type "ConceptNode"))
		(SetLink
			(Glob "$x")
			(Concept "apple"))
		(ListLink
			(Glob "$x"))))

; -----------------------------------------------------------------
; Backtrack + black box link
(Evaluation
	(Predicate "Some Seq")
	(List
		(Concept "Some Node")
		(List
			(Concept "A")
			(Concept "B")
			(Concept "C")
			(Concept "D")
			(Concept "E")
			(Concept "F")
			(Concept "G")
			(Concept "H")
			(Concept "I"))))

; Only returns true if ATOM is "C"
(define-public (match-c ATOM)
	(if (equal? (Concept "C") ATOM)
		(stv 1 1)
		(stv 0 1)))

; Only returns true if ATOM is "DEF"
(define-public (match-def ATOM)
	(if (equal? (List (Concept "D") (Concept "E") (Concept "F")) ATOM)
		(stv 1 1)
		(stv 0 1)))

; The situation below is that there are many different ways
; to ground all these three globs, but the black-box link
; accepts only one of them, so make sure we can backtrack
; until we find a match for the whole pattern.
(define backtrack
	(Bind
		(VariableList
			(TypedVariable (Glob "$x")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1))))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 1) (Number 1))))
			(TypedVariable (Glob "$z")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1)))))
		(And
			(List (Glob "$x") (Glob "$y") (Glob "$z"))
			(Evaluation (GroundedPredicate "scm: match-c") (List (Glob "$y"))))
		(List
			(List (Glob "$x"))
			(List (Glob "$y"))
			(List (Glob "$z")))))

(define backtoo
	(Bind
		(VariableList
			(TypedVariable (Glob "$x")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1))))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 1) (Number -1))))
			(TypedVariable (Glob "$z")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1)))))
		(And
			(List (Glob "$x") (Glob "$y") (Glob "$z"))
			(Evaluation (GroundedPredicate "scm: match-def")
				(List (List (Glob "$y")))))
		(List
			(List (Glob "$x"))
			(List (Glob "$y"))
			(List (Glob "$z")))))

(define backmore
	(Bind
		(VariableList
			(TypedVariable (Glob "$x")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1))))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 1) (Number -1))))
			(TypedVariable (Glob "$z")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1)))))
		(And
			(Evaluation (Predicate "Some Seq")
				(List (Concept "Some Node")
					(List (Glob "$x") (Glob "$y") (Glob "$z"))))
			(Evaluation (GroundedPredicate "scm: match-def")
				(List (List (Glob "$y")))))
		(List
			(List (Glob "$x"))
			(List (Glob "$y"))
			(List (Glob "$z")))))

; -----------------------------------------------------------------
; The same glob appears more than once in the pattern

(ListLink
	(ConceptNode "a")
	(ConceptNode "b")
	(ConceptNode "c")
	(ConceptNode "d")
	(ConceptNode "e")
	(ConceptNode "f")
	(ConceptNode "g")
	(ConceptNode "h")
	(ConceptNode "i")
	(ConceptNode "j")
	(ConceptNode "k"))

(ListLink
	(ConceptNode "a")
	(ConceptNode "b")
	(ConceptNode "FOO")
	(ConceptNode "e")
	(ConceptNode "f")
	(ConceptNode "FOO")
	(ConceptNode "j")
	(ConceptNode "k"))

(ListLink
	(ConceptNode "a")
	(ConceptNode "b")
	(ConceptNode "FOO")
	(ConceptNode "e")
	(ConceptNode "f")
	(ConceptNode "FOO")
	(ConceptNode "BAR")
	(ConceptNode "j")
	(ConceptNode "k"))

(define get-ma
	(GetLink
		(GlobNode "star")
		(ListLink
			(ConceptNode "a")
			(ConceptNode "b")
			(GlobNode "star")
			(ConceptNode "e")
			(ConceptNode "f")
			(GlobNode "star")
			(ConceptNode "j")
			(ConceptNode "k"))))
