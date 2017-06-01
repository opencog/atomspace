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

(ListLink (Concept "I") (Concept "love") (Number 42))

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
; Globs can have both type and interval restrictions by using TypeSetLink
; type == ConceptNode and interval == zero to infinity

(define love-typeset-glob
	(BindLink
		(TypedVariable (Glob "$star")
			(TypeSetLink (IntervalLink (Number 0) (Number -1)) (Type "ConceptNode")))
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
				(TypeSetLink (Type "ConceptNode") (IntervalLink (Number 1) (Number 1))))
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
