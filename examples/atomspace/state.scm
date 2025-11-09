;
; state.scm - Setting state in the atomspace.
;
; This demonstrates using the StateLink to maintain unique state in the
; atomspace. A StateLink is a kind of link of which there can only ever
; be one of in the atomspace.  Whenever a new StateLink is added to the
; atomspace, the old one is automatically removed.
;
; StateLinks can be used to associate properties with atoms; they behave
; like atomic key-value pairs, allowing one and only one value to be
; associated with a given key. They are literally "atomic", in the
; multi-threaded programming sense: their update is protected by a mutex
; lock, and so the state remains single-valued, even when viewed from
; multiple threads.
;
; See the `property.scm` example for a more complex, but also more
; practical way of defining properties for knowledgebases that want
; to view the world in terms of key-value pairs.  For rapidly-changing
; values, such as video and audio streams, there is a different
; mechanism, given in later examples.

(use-modules (opencog) (opencog exec))

; The current state of "fruit" is "apple".
(State (Anchor "fruit") (Concept "apple"))

; Lets make sure of that:
(cog-incoming-set (Anchor "fruit"))

; Change the state to banana:
(State (Anchor "fruit") (Concept "banana"))

; Lets make sure the state changed:
(cog-incoming-set (Anchor "fruit"))

; Change it back to apple:
(State (Anchor "fruit") (Concept "apple"))

; Lets make sure the state changed:
(cog-incoming-set (Anchor "fruit"))

; Query the current state, using the MeetLink from prior examples.
; The MeetLink always returns a UnisetValue of everything it found; in this
; case, the set is a singleton, and the need for this wrapping is a
; bit superfluous. Oh well.
(cog-execute! (Meet (State (Anchor "fruit") (Variable "$x"))))

; Lets look at the state again:
(cog-incoming-set (Anchor "fruit"))

; Ahhh! This time, there are two StateLinks for "fruit"!  How can that
; be?  Didn't StateLink promise to be unique? Well, yes, it did, but
; only when the state is fully grounded, i.e. is a "ground term" having
; no variables in it.  So, StateLinks still operate as expected: they
; associate a single key to a single ground-term. An unlimited number
; of ungrounded terms are allowed, so that arbitrary queries can be
; written, just like the above.

; ------------------
; Execute, to see if the current state is really is "apple": this
; should return a truth value corresponding to "true". Here, EqualLink
; is a link that, when executed, returns a TruthValue.

(cog-execute!
	(EqualLink
		(Set (Concept "apple"))
		(CollectionOf (Meet (State (Anchor "fruit") (Variable "$x"))))))

; Same as above, but should be false:
(cog-execute!
	(EqualLink
		(Set (Concept "banana"))
		(CollectionOf (Meet (State (Anchor "fruit") (Variable "$x"))))))

; Change the state, using PutLink:
(cog-execute!
	(Put
		(State (Anchor "fruit") (Variable "$x"))
		(Concept "strawberry")))

; And, again verify that the state has changed, as expected:
(cog-execute!
	(Meet (State (Anchor "fruit") (Variable "$x"))))

; ------------------
; Hang on; apples, bananas and strawberries are all fruit. How can
; this be expressed? Why, just as before:

(Edge (Predicate "fruit") (List (Concept "apple")))
(Edge (Predicate "fruit") (List (Concept "banana")))
(Edge (Predicate "fruit") (List (Concept "strawberry")))

; By convention, one uses a PredicateNode here, instead of an
; AnchorNode. One could do it the other way around, but this is the
; current convention. This convention mostly just makes it easier to
; understand the atomspace contents.
;
; The message here is that Edges are multi-state StateLinks.
; Really. So, for example:

(cog-execute! (Meet (Edge (Predicate "fruit") (Variable "$x"))))

; ------------------
; Only one problem: the above example is a bad example. We should
; instead have said "is-a". The whole world knows the "is-a" relation.

(Edge (Predicate "Is A") (List (Concept "fruit") (Concept "apple")))
(Edge (Predicate "Is A") (List (Concept "fruit") (Concept "banana")))
(Edge (Predicate "Is A") (List (Concept "fruit") (Concept "strawberry")))

(cog-execute! (Meet
	(Edge (Predicate "Is A") (List (Concept "fruit") (Variable "$x")))))

; ------------------
; The is-a relation is so very special, it gets it's own custom link
; type. It is a bit shorter and easier to read.

(Inheritance (Concept "fruit") (Concept "apple"))
(Inheritance (Concept "fruit") (Concept "banana"))
(Inheritance (Concept "fruit") (Concept "strawberry"))

(cog-execute! (Meet (Inheritance (Concept "fruit") (Variable "$x"))))

; By convention, one writes a ConceptNode instead of a PredicateNode
; in this situation. Again, this is just a convention. Its handy, and
; makes things more readable.

; ------------------
; One can also think of "fruit" as a set, with lots of members in it.
; The SetLink used so far has been "anonymous", it is a set without a
; name.  The MemberLink offers a way to name name a set:

(Member (Concept "apple")     (Concept "fruit"))
(Member (Concept "banana")   (Concept "fruit"))
(Member (Concept "strawberry") (Concept "fruit"))

(cog-execute! (Meet (Member (Variable "$x") (Concept "fruit"))))

; By convention, this is backwards from the InheritanceLink. It is meant
; to be read "first thing is a member of the set that is the second thing".

; ------------------
; There is no such thing as "InheritanceStateLink" or "MemberStateLink".
; These are not needed; having them would be confusing. The StateLink is
; ideal for associating a predicate to a single-valued grounded term.
; The EdgeLink is for associating multiple things together, in a
; naive-set-theory predicate-like way. The InheritanceLink is handy for
; the extremely common "is-a" relation.  The MemberLink is handy for the
; equally-common set-membership relation.
