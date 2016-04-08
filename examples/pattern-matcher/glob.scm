;
; glob.scm
;
; Demonstration of globbing.
;
; GlobNodes are like VariableNodes, except that they can match multiple
; atoms in a sequence.  By contrast, a single VariableNode can match
; only a single atom at a time.  Thus, globs resemble the * character
; in regular expressions (regexes).
;
; To be precise: Globs are like regex + (1 or more in a sequence) and
; not regex * (zero or more in sequence).  A GlobNode has to match at
; least one atom. At this time, there is no atomese equivalent to 
; regex ? (zero or one matches) nor to the * (zero or more matches).
; This may change; ask on the mailing list or open a github feature
; request.

(use-modules (opencog) (opencog exec))

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
