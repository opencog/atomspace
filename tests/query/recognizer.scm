
; Pattern recognition is the dual of pattern matching.

(use-modules (opencog))
(use-modules (opencog query))

; Two different pseudo-AIML rules:
;    I * you   --> I * you too
;    I love *  --> I like * a lot!
;
(define star-you
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

(define love-star
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
			(ConceptNode "lot!"))))

;-------------------------------------------------------
(define sent
	;; A pretend "sentence" that is the "input".
	(PatternLink
		(BindLink
			(ListLink
				(ConceptNode "I")
				(ConceptNode "love")
				(ConceptNode "you"))
			(VariableNode "$impl"))))

;; Search for patterns that match the sentence. Both of the above
;; should match.
; (cog-recognize sent)

;-------------------------------------------------------
;; Another sentence, but with adverbs.  It will match one of the
;; patterns, but not the other.
(define adv-sent
	(PatternLink
		(BindLink
			(ListLink
				(ConceptNode "I")
				(ConceptNode "really")
				(ConceptNode "truly")
				(ConceptNode "love")
				(ConceptNode "you"))
			(VariableNode "$impl"))))

;; Perform the search.
; (cog-recognize adv-sent)
