
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
		(GlobNode "$star")
		(ConceptNode "you"))
	(ListLink
		(ConceptNode "I")
		(GlobNode "$star")
		(ConceptNode "you")
		(ConceptNode "too")))

(BindLink
	(ListLink
		(ConceptNode "I")
		(ConceptNode "love")
		(GlobNode "$star"))
	(ListLink
		(ConceptNode "I")
		(ConceptNode "like")
		(GlobNode "$star")
		(ConceptNode "a")
		(ConceptNode "lot!")))

(define data
	;; A pretend "sentence" that is the "input".
	(PatternLink
		(BindLink
			(ListLink
				(ConceptNode "I")
				(ConceptNode "love")
				(ConceptNode "you"))
			(VariableNode "$impl"))))

;; Perform the search.
(cog-recognize data)

;; At this time, the above will return the below:
;; The BindLinks are NOT evaluated!  To evaluate, see bottom

(SetLink
	(BindLink
		(ListLink
			(ConceptNode "I")
			(GlobNode "$star")
			(ConceptNode "you")
		)
		(ListLink
			(ConceptNode "I")
			(GlobNode "$star")
			(ConceptNode "you")
			(ConceptNode "too")
		)
	)
	(BindLink
		(ListLink
			(ConceptNode "I")
			(ConceptNode "love")
			(GlobNode "$star")
		)
		(ListLink
			(ConceptNode "I")
			(ConceptNode "like")
			(GlobNode "$star")
			(ConceptNode "a")
			(ConceptNode "lot!")
		)
	)
)

;; Evaluate each of the bind links that were found.
(define ruleset (cog-recognize data))

(map cog-bind (cog-outgoing-set ruleset))

; Which returns the below:
((SetLink
   (ListLink
      (ConceptNode "I")
      (ConceptNode "love")
      (ConceptNode "you")
      (ConceptNode "too")))
 (SetLink
   (ListLink
      (ConceptNode "I")
      (ConceptNode "like")
      (ConceptNode "you")
      (ConceptNode "a")
      (ConceptNode "lot!"))))


