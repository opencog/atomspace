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
; some function "procedurally"), one can also maintain the knowledgebase
; with knowledge-snippets themselves. That is, the tools for maintaining
; the data can be a part of the data itself.
;
; The GetLink and PutLink, from the `get-put.scm` example, provide the
; tools needed to work with facts being deduced on the fly. They allow
; facts to be asserted and retracted, even though their specific present
; form is not yet known.
;
; The `cog-execute!` function is the primary driver for affecting state
; change. Every time it is called, the state of the AtomSpace will
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

(use-modules (opencog) (opencog exec))

; A utility function to print all EvaluationLinks in the AtomSpace.
; Very handy to see what is in the AtomSpace. Don't worry about the
; scheme code used to implement this: it's just a black box, and is
; not really a part of this example. Just know that it prints all
; EvaluationLinks.
(define (show-eval-links)
	(cog-map-type (lambda (h) (display h) #f) 'EvaluationLink))

; Verify that the AtomSpace contains no EvaluationLinks:
(show-eval-links)

; Define a beta-reduction, using the PutLink. The EvaluationLink won't
; be added until this is reduced. When it is reduced, the ListLink will
; be substituted for the variable $x, creating the fully-assembled
; EvaluationLink.
;
(define to-be-added
	(Put
		(Evaluation
		    (Predicate "some property") (Variable "$x"))
		(ListLink
			(Concept "thing A")
			(Concept "B-dom-ness"))))

; The AtomSpace now contains one ungrounded EvaluationLink.
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
; AtomSpace. A DeleteLink must ALWAYS have at least one variable in
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
; You might have noticed by now that Atomese is verbose, and sometimes
; hard to read.  This is not an accident. Atomese is not intended for
; humans. It is intended for machines, for algorithms. The point of all
; of these structures, of the contents of the AtomSpace, is that various
; different kinds of learning and reasoning algorithms will run on it,
; and mangle the data in all kinds of ways.
;
; In order for this to work, the contents of the AtomSpace have to be
; "easy" for the algorithms to mangle.  So, graph re-writing works only
; when everything is a graph. Beta reduction works only when everything
; is reducible.  This makes writing programs in Atomese feel like
; writing programs in assembly code.  Famously, assembly code is not
; for humans, but machines love it. So you've secretly been studying
; a tutorial in assembly. Most users should eventually refocus on some
; other, higher-level layer.
;
; Anyway, to make Atomese easier to read (for humans), the DefineLink
; can be used as a stand-in for predefined collections of atoms. Its
; a lot like `define` in scheme, but different cause its a graph.
; Actually, its a lot like StateLink, except that, after defining it,
; you cannot change the definition. Its frozen forever. (Well, maybe
; you can delete it, but only if no one else is using it; otherwise,
; it is not even deletable).
;
; DefineLink can be used to specify the body of a PutLink.
; Thus, for example:

(Define
	(DefinedSchema "colored things")
	(Lambda (Inheritance (Variable "$yyy") (Concept "color"))))

(cog-execute!
	(PutLink
		(DefinedSchema "colored things")
		(Concept "green")))

; This will cause the following to be created:
;
; (InheritanceLink
;    (ConceptNode "green")
;    (ConceptNode "color"))

; The above example introduces several other new things, the most
; important being the LambdaLink. The LambdaLink is a lot like the
; lambda in scheme, LISP or lambda-calculus. But it's different,
; because AtomSpace contents are graphs, and are not strings-of-symbols.
; This difference is subtle, and can lead to tricky confusions,
; seeming paradoxes and generally-annoying properties. Lambda and
; graphs are not quite natural partners.
;
; Anyway: at the simplest level, LambdaLink can be thought of as a
; generic, agnostic way of converting free variables into bound
; variables. A variable inside of a Lambda belongs to that Lambda,
; it does not "leak" outside, and it is freely alpha-convertible to
; other variable names.
;
; The Lambda is agnostic, in that it has many special cases: so,
; "for all x ...", "there exists x ...", "sum over all n ...",
; "integral over all dx ..." are all examples of Lambdas.  In
; the Atomese type hierarchy, GetLink, PutLink, BindLink and DeleteLink
; are all derived from LambdaLink, in that they can all have a header
; that defines the collection of variables that will appear in the body.
