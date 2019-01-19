;
; state.scm - Setting state in the atomspace.
;
; This demonstrates using the StateLink to maintain unique state in the
; atomspace. A Statelink is a kind of link of which there can only ever
; be one of in the atomspace.  Whenever a new StateLink is added to the
; atomspace, the old one is automatically removed.
;
; StateLinks can be used to associate properties with atoms; they behave
; like atomic key-value pairs, allowing one and only one value to be
; associated with a given key. They are literally "atomic", in the
; multi-threaded programming sense: thier update is protected by a mutex
; lock, and so the state remains single-valued, even when viewed from
; multiple threads.
;
; See the `property.scm` example for a more complex, but also more
; practical way of defining properties for knowledgebases that want
; to view the world in terms of key-value pairs.  For rapidly-changing
; values, such as video and audio streams, there is a different
; mechanism, given in later examples.

(use-modules (opencog) (opencog exec) (opencog query))

; The current state of "fruit" is "apple".
(State (Anchor "fruit") (Concept "apple"))

; Lets make sure of that:
(cog-incoming-set (Anchor "fruit"))

; Change the state to bananna:
(State (Anchor "fruit") (Concept "bananna"))

; Lets make sure the state changed:
(cog-incoming-set (Anchor "fruit"))

; Change it back to apple:
(State (Anchor "fruit") (Concept "apple"))

; Lets make sure the state changed:
(cog-incoming-set (Anchor "fruit"))

; Query the current state, using the GetLink from prior examples.
; The GetLink always returns a SetLink of everything it found; in this
; case, the set is a singleton, and the need for this wrapping is a
; bit superfluous. Oh well.
(cog-execute! (Get (State (Anchor "fruit") (Variable "$x"))))

; Lets look at the state again:
(cog-incoming-set (Anchor "fruit"))

; Ahhh! This time, there are two StateLinks for "fruit"!  How can that
; be?  Didn't StateLink promise to be unique? Well, yes, it did, but
; only when the state is full grounded, i.e. is a "ground term" having
; no variables in it.  So, StateLinks still operate as expected: they
; associate a single key to a single ground-term. An unlimited number
; of ungrounded terms are allowed, so that arbitrary queries can be
; written, just like the above.

; ------------------
; The `cog-execute!` function always returns atoms. The `cog-evaluate!`
; function always returns TruthValues. Very roughly speaking,
; "execution" is like running a program, "evaluation" is like evaluating
; an expression. Very similar, but not the same in Atomese.
;
; So, evaluate, to see if the current state is really is "apple": this
; should return a truth value corresponding to "true". Here, EqualLink
; is a link that, when evaluated, returns a TruthValue.

(cog-evaluate!
	(EqualLink
		(Set (Concept "apple"))
		(Get (State (Anchor "fruit") (Variable "$x")))))

; Same as above, but should be false:
(cog-evaluate!
	(EqualLink
		(Set (Concept "bananna"))
		(Get (State (Anchor "fruit") (Variable "$x")))))

; Change the state, using PutLink:
(cog-execute!
	(Put
		(State (Anchor "fruit") (Variable "$x"))
		(Concept "strawberry")))

; And, again verify that the state has changed, as expected:
(cog-execute!
	(Get (State (Anchor "fruit") (Variable "$x"))))
