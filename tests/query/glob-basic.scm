;
; glob-basic.scm
;

(use-modules (opencog))
(use-modules (opencog query))

;;; Populate the atomspace with some "sentences".
(ListLink
	(ConceptNode "I")
	(ConceptNode "love")
	(ConceptNode "you"))

(ListLink
	(ConceptNode "I")
	(ConceptNode "really")
	(ConceptNode "totally")
	(ConceptNode "need")
	(ConceptNode "you"))

(ListLink
	(ConceptNode "I")
	(ConceptNode "love")
	(ConceptNode "teddy")
	(ConceptNode "bears")
	(ConceptNode "a")
	(ConceptNode "lot"))

;; Two different re-write rules. The first rule, immediately below,
;; says "I * you" -> "I * you too".
(define glob-you
	(BindLink
	(ListLink
		(ConceptNode "I")
		(GlobNode "$star")
		(ConceptNode "you"))
	(ListLink
		(ConceptNode "I")
		(GlobNode "$star")
		(ConceptNode "you")
		(ConceptNode "too"))))

;; This one implements "I love *" -> "Hey! I love * too"
(define love-glob
	(BindLink
	(ListLink
		(ConceptNode "I")
		(ConceptNode "love")
		(GlobNode "$star"))
	(ListLink
		(ConceptNode "Hey!")
		(ConceptNode "I")
		(ConceptNode "like")
		(GlobNode "$star")
		(ConceptNode "also"))))

;; Both of these patterns should "work as expected".
; (cog-bind glob-you)
; (cog-bind love-glob)
