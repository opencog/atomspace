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
	(ConceptNode "need")
	(ConceptNode "you"))

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

(use-modules (opencog logger))
(cog-logger-set-stdout #t)
(cog-logger-set-level "fine")

(cog-bind globby)
