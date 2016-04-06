;
; recog.scm
;
; This is a rough sketch of the idea that pattern recognition
; is the dual of pattern matching.  There are many things wrong
; with the below; its just a sketch. However, it does work.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog query))

; Two different pseudo-AIML rules:
;    I * you   --> I * you too
;    I love *  --> I like * a lot!
;
(BindLink
	(ListLink
		(Concept "I") (Glob "$star") (Concept "you"))
	(ListLink
		(Concept "I") (Glob "$star") (Concept "you") (Concept "too")))

(BindLink
	(ListLink
		(Concept "I")
		(Concept "love")
		(Glob "$star"))
	(ListLink
		(Concept "I")
		(Concept "like")
		(Glob "$star")
		(Concept "a")
		(Concept "lot!")))

;-------------------------------------------------------
;; A pretend "sentence" that is the "input".
(define sent
	(ListLink (Concept "I") (Concept "love") (Concept "you")))

;; Search for patterns that match the sentence. Both of the above
;; should match.
(cog-execute! (DualLink sent))

;; The above should return the below:
;; The BindLinks are NOT evaluated!  To evaluate, see bottom.
(SetLink
	(BindLink
		(ListLink
			(Concept "I")
			(Glob "$star")
			(Concept "you")
		)
		(ListLink
			(Concept "I")
			(Glob "$star")
			(Concept "you")
			(Concept "too")
		)
	)
	(BindLink
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star")
		)
		(ListLink
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "a")
			(Concept "lot!")
		)
	)
)

;-------------------------------------------------------
;; Another sentence, but with adverbs.  It will match one of the
;; patterns, but not the other.
(define adv-sent
	(PatternLink
		(BindLink
			(ListLink
				(Concept "I")
				(Concept "really")
				(Concept "truly")
				(Concept "love")
				(Concept "you"))
			(Variable "$impl"))))

;; Perform the search.
(cog-recognize adv-sent)

;-------------------------------------------------------
;; Evaluate each of the bind links that were found.
(define ruleset (cog-recognize sent))

(map cog-bind (cog-outgoing-set ruleset))

; For the non-adverbial sentence this returns the below:
((SetLink
   (ListLink
      (Concept "I")
      (Concept "love")
      (Concept "you")
      (Concept "too")))
 (SetLink
   (ListLink
      (Concept "I")
      (Concept "like")
      (Concept "you")
      (Concept "a")
      (Concept "lot!"))))

;-------------------------------------------------------

; A pattern with two globs in it.
; The types of the globs are constrained, because, if not constrained
; the globs can sometimes pick up on parts of the various patterns
; created above.  We really want them to only pick up on the "sentences"
; (strings of Concepts).
(define a-love-b
	(BindLink
		(VariableList
			(TypedVariable (Glob "$A") (Type "Concept"))
			(TypedVariable (Glob "$B") (Type "Concept")))
		(ListLink
			(Glob "$A")
			(Concept "love")
			(Glob "$B"))
		(ListLink
			(Concept "I'm")
			(Concept "sure")
			(Concept "that")
			(Glob "$A")
			(Concept "love")
			(Glob "$B"))))

; Lets see if the above can be found!
(cog-recognize adv-sent)

(define constrained-adv-sent
	(PatternLink
		(BindLink
			(Variable "$type constraints")
			(ListLink
				(Concept "I")
				(Concept "really")
				(Concept "truly")
				(Concept "love")
				(Concept "you"))
			(Variable "$impl"))))

(cog-recognize constrained-adv-sent)

;-------------------------------------------------------
*unspecified*
