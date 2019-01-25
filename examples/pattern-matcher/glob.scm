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
; least one atom. At this time, there is no Atomese equivalent to
; regex ? (zero or one matches) nor to the * (zero or more matches).
; This may change; ask on the mailing list or open a github feature
; request.

(use-modules (opencog) (opencog exec))

;;; Populate the AtomSpace with some "sentences".
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


;;; Populate the AtomSpace with some more "sentences".
(ListLink (Concept "I") (Concept "love") (Number 42))

; This will find the match (Number 42); it will NOT match (Concept "you")
; (cog-execute! love-type-glob)

; -----------------------------------------------------------------
; Globs can have interval restriction

(define love-interval-glob
    (BindLink
        (TypedVariable (Glob "$star") (IntervalLink (Number 0) (Number 1)))
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

; This will find both (Number 42) and (Concept "like") but not anything else
; as they are not in the interval of zero to one
; (cog-execute! love-interval-glob)

; -----------------------------------------------------------------
; Globs can have both type and interval restrictions by using TypeSetLink

(define love-typeset-glob
    (BindLink
        (TypedVariable (Glob "$star")
            (TypeSetLink (IntervalLink (Number 0) (Number -1)) (Type "ConceptNode")))
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

; This will find only ConceptNodes, and the interval is zero to infinity
; so it should find both (Concept "like") and
; (Concept "teddy") (Concept "bear") (Concept "a") (Concept "lot")
; (cog-execute! love-typeset-glob)

; -----------------------------------------------------------------
; Slightly more complicated

; Populate the AtomSpace for the next test case
(ListLink
    (Concept "I")
    (Concept "need")
    (Concept "you")
    (Concept "now"))

(ListLink
    (Concept "they")
    (Concept "think")
    (Concept "I")
    (Concept "hate")
    (Concept "you"))

; Here we have three GlobNodes
; $x can be grounded to nothing or as many as possible
; $y has to be grounded to one and only one ConceptNode
; $z can be grounded to nothing or as many as possible
(define love-three-globs
    (BindLink
        (VariableList
            (TypedVariable (Glob "$x") (IntervalLink (Number 0) (Number -1)))
            (TypedVariable (Glob "$y")
                (TypeSetLink (Type "ConceptNode") (IntervalLink (Number 1) (Number 1))))
            (TypedVariable (Glob "$z") (IntervalLink (Number 0) (Number -1))))
        (ListLink
            (Glob "$x")
            (Concept "I")
            (Glob "$y")
            (Concept "you")
            (Glob "$z"))
        (ListLink
            (Concept "Hey!")
            (Concept "I")
            (Glob "$y")
            (Concept "you")
            (Concept "also"))))

; This will find three possible groundings
; 1) $x = (Concept "they") (Concept "think")
;    $y = (Concept "hate")
;    $z = <none>
;
; 2) $x = <none>
;    $y = (Concept "need")
;    $z = (Concept "now")
;
; 3) $x = <none>
;    $y = (Concept "love")
;    $z = <none>
; (cog-execute! love-three-globs)
