
; Some very crude experiments
;
; This is a rough sketch of the idea that pattern recognition
; is the dual of pattern matching.  There are many things wrong
; with the below; its just a sketch.

; Two different pseudo-AIML rules:
;    I * you   --> I * you too
;    I love *  --> I like * a lot!
;
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

(define data
	;; A pretend "sentence" that is the "input".
	(PatternLink
		(ListLink
			(ConceptNode "I")
			(ConceptNode "love")
			(ConceptNode "you"))))

;; Perform the search.
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
