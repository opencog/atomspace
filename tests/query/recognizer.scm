
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
;-------------------------------------------------------

; A pattern with two globs in it.
(define a-hate-b
	(BindLink
		(ListLink
			(GlobNode "$A")
			(ConceptNode "hates")
			(GlobNode "$B"))
		(ListLink
			(ConceptNode "I'm")
			(ConceptNode "sure")
			(ConceptNode "that")
			(GlobNode "$A")
			(ConceptNode "hates")
			(GlobNode "$B"))))

(define hate-speech
	;; A pretend "sentence" that should trigger the above.
	(PatternLink
		(BindLink
			(ListLink
				(ConceptNode "Mike")
				(ConceptNode "really")
				(ConceptNode "hates")
				(ConceptNode "Sue")
				(ConceptNode "a")
				(ConceptNode "lot"))
			(VariableNode "$impl"))))

;-------------------------------------------------------

*unspecified*
