;
; assert-retract.scm -- Asserting and retracting facts.
;
; A knowledgebase is rarely static. one needs to be able to assert
; new facts, and retract old ones.  Of course, one can always do this
; directly: just write some Atoms at the guile prompt, or just delete
; them (using the `cog-delete` function). But what if you do not yet
; know what these facts are? What if they are the result of logical
; reasoning?
;
; Rather than writing a procedural program that performs reasoning,
; and then just performs the insertion/deletion "manually" (by calling
; some function "proceedurally"), one can also maintain the knowlegbase
; with knowledge-snippets themselves. That is, the tools for maintaining
; the data can be a part of the data itself.
;
; The GetLink and PutLink, from the `get-put.scm` example, provide the
; tools needed to work with facts beig deduced on the fly. They allow
; facts to be asserted and retracted, even though thier specific present
; form is not yet known.
;
; The `cog-execute!` function is the primary driver for affecting state
; change. Every time it is called, the state of the atomspace will
; (usually) change. The `cog-execute!` forces a single time-step of
; an Atomese knowledgebase into it's next form.  In this sense, the
; knowledgebase is a dynamical system. It is not just a static
; collection of facts. It is also a collection of rules that determine
; how the state changes over time.
;
; The idea of asserting and retracting facts is taken from ProLog, where
; the collection of facts, as a whole, behaves like a database (this is
; sometimes called the "DataLog" subset of ProLog). There must be a way
; of adding records, or removing them from the database.  So, likewise,
; in the AtomSpace: the AtomSpace is a database, a knowledgebase.
;
; In this example, PutLink is used to assert new knowledge. The
; DeleteLink is used to remove it. The DeleteLink is quite special:
; it is one of a collection of Link types that MUST have a VariableNode
; in it.  That is, DeleteLinks can NEVER be "ground terms", as, by
; definition, grounding it causes it to vanish.

(use-modules (opencog) (opencog exec) (opencog query))

; A utility function to print all EvaluationLinks in the AtomSpace.
; Very handy to see what is in the AtomSpace. Don't worry about the
; scheme code used to implement this: it's just a black box, and is
; not really a part of this example. Just know that it prints all
; EvaluationLinks.
(define (show-eval-links)
	(cog-map-type (lambda (h) (display h) #f) 'EvaluationLink))

; Verify that the atomspace contains no EvaluationLinks:
(show-eval-links)

; Define a beta-reduction, using the PutLink. The EvaluationLink won't
; be added until this is reduced. When it is reduced, the ListLink will
; be substitited for the variable $x, creating the fully-assembled
; EvaluationLink.
;
(define to-be-added
	(Put
		(Evaluation
		    (Predicate "some property") (Variable "$x"))
		(ListLink
			(Concept "thing A")
			(Concept "B-dom-ness"))))

; The atomspace now contains one ungrounded EvaluationLink.
; (Its called "ungrounded" because it has a free variable in it).
(show-eval-links)

; Now, actually create the EvaluationLink.
(cog-execute! to-be-added)

; Take a look again:
(show-eval-links)

; Whatever the PutLink does, the GetLink can un-do.  The GetLink
; below has a satisfying set that corresponds to the PutLink above.
; (This means that Get and Put are "adjoint functors".)

(define get-property
	(Get (Evaluation (Predicate "some property") (Variable "$x"))))

; The cog-execute! function will return the set of all atoms that
; the GetLink finds.
(cog-execute! get-property)

; The PutLink below causes the PutLink above to be un-done.
; It explicitly specifies the same parts as were specified before,
; but when these parts are assembled into a whole, they materialize
; inside of a DeleteLink, which causes them to disappear. That is,
; it is impossible to insert a fully-grounded DeleteLink into the
; atomspace. A DeleteLink must ALWAYS have at least one variable in
; it, otherwise, it cannot exist. Attempting such an insertion will
; cause the body of the DeleteLink to be removed.
;
; (This shows that PutLink and DeleteLink are also adjoint functors.)
;
(define remove-thing-ab
	(Put
		(Delete
			(Evaluation (Predicate "some property") (Variable "$x")))
		(ListLink (Concept "thing A") (Concept "B-dom-ness"))))

; Force its removal.
(cog-execute! remove-thing-ab)

; Look for it; it should be absent.
(cog-execute! get-property)
; Double-check it's absence.
(show-eval-links)

; Add it back in:
(cog-execute! to-be-added)
(cog-execute! get-property)

; ... and so on. We can now continue to remove it and add it
; back in repeatedly.
(cog-execute! remove-thing-ab)
(cog-execute! get-property)
(cog-execute! to-be-added)
(cog-execute! get-property)


; It is also useful to generically remove any atom matching
; a pattern description. This can be done by combining the
; PutLink with a GetLink performing a query. The below uses
; the GetLink to find groundings for the variable $x, and then
; passes those groundings to the PutLink/DeleteLink combination,
; which removes them.
;
(define remove-some-property
	(PutLink
		(DeleteLink
			(EvaluationLink
				(PredicateNode "some property")
				(VariableNode "$x")))
		(GetLink
			(EvaluationLink
				(PredicateNode "some property")
				(VariableNode "$x")))))

; Now, remove the EvaluationLink
(cog-execute! remove-some-property)
(cog-execute! get-property)

; We can now add and remove over and over:
(cog-execute! to-be-added)
(cog-execute! get-property)

(cog-execute! remove-some-property)
(cog-execute! get-property)

; And do it again, for good luck:
(cog-execute! to-be-added)
(cog-execute! get-property)
(cog-execute! remove-some-property)
(cog-execute! get-property)


; ------------------------------------------------
; The simplest way to combine Delete/Get/Put to maintain state is to
; use the StateLink.  StateLinks do not even have to be executed;
; simply using them changes the state. See the `state.scm` example
; for details.
;
; StateLinks can be thought of as a combined Delete+Put. Whenever the
; state is set, the old state is deleted, first. Computationally, this
; is an atomic operation: it is protected by a mutex lock (so you can
; safely use it in multi-threaded Atomese programs).
;
; The StateLink can be thought of as a key-value pair.  For any given
; key, there is a corresponding value. It is kind of UniqueLink: the
; StateLink can only ever correspond to one value at a time, never two.
; Compare to EvaluationLink: you can create as many of those as you
; want; but there can ever be only one state. States are necessarily
; single-valued. (They are fermions to EvaluationLink's bosons.)
;
(StateLink
	(PredicateNode "some property")
	(ListLink
		(ConceptNode "thing A")
		(ConceptNode "alternative B")))

(define get-state
	(GetLink
		(StateLink
			(PredicateNode "some property")
			(VariableNode "$x"))))

(cog-execute! get-state)

(StateLink
	(PredicateNode "some property")
	(ListLink
		(ConceptNode "thing A")
		(ConceptNode "The V alternative")))

(cog-execute! get-state)

(StateLink
	(PredicateNode "some property")
	(ListLink
		(ConceptNode "thing A")
		(ConceptNode "first alternative again")))

(cog-execute! get-state)

; ... and so on, ad infinitum

; ------------------------------------------------
; DefineLink can be used to specify the body of a PutLink.
; Thus, for example:

(DefineLink
	(DefinedSchemaNode "colored things")
	(LambdaLink
		(InheritanceLink
			(ConceptNode "color")
			(VariableNode "$yyy"))))

(cog-execute!
	(PutLink
		(DefinedSchemaNode "colored things")
		(ConceptNode "green")))

; Will cause the following to be created:
;
; (InheritanceLink
;    (ConceptNode "color")
;    (ConceptNode "green"))
