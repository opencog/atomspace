
; Some very crude experiments
;
; This is a rough sketch of the idea that pattern recognition
; is the dual of pattern matching.  There are many things wrong
; with the below; its just a sketch.

(define data
	(PatternLink
		(ListLink
			(ConceptNode "I")
			(ConceptNode "love")
			(ConceptNode "you"))))

(BindLink
	(ListLink
		(ConceptNode "I")
		(VariableNode "$star")
		(ConceptNode "you"))
	(ListLink
		(ConceptNode "I")
		(VariableNode "$star")
		(ConceptNode "you")
		(ConceptNode "too")))

(BindLink
	(ListLink
		(ConceptNode "I")
		(ConceptNode "love")
		(VariableNode "$star"))
	(ListLink
		(ConceptNode "I")
		(ConceptNode "like")
		(VariableNode "$star")
		(ConceptNode "a")
		(ConceptNode "lot!")))

(cog-recognize data)

;; At this time, the above will return this:
(SetLink
   (ListLink
      (ConceptNode "I")
      (VariableNode "$star")
      (ConceptNode "you")
   )
   (ListLink
      (ConceptNode "I")
      (ConceptNode "love")
      (VariableNode "$star")
   )
)

;; It is not directy useful, because the rest of the BindLink
;; involved is not located and specified.  But it illustrates
;; the general idea.
;;
