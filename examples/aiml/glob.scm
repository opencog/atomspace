;
; glob.scm
;
; Demonstration of globbing.
;

(use-modules (opencog))
(use-modules (opencog query))

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


;;; Populate the atomspace with some more "sentences".
(ListLink (Concept "I") (Concept "love") (Number 42))

; This will find the match (Number 42); it will NOT match (Concept "you")
; (cog-execute! love-type-glob)
