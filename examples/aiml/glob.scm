;
; glob.scm
;
; Demonstration of globbing.  Under development, broken.
;

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

(define globby
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

(define glob-end
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
		(ConceptNode "too"))))

(use-modules (opencog logger))
(cog-logger-set-stdout #t)
(cog-logger-set-level "fine")

(cog-bind globby)
; (cog-bind glob-end)
