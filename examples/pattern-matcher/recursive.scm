;
; recursive.scm -- A recursive chain of queries.
;
; This demo illustrates how recursive queries can be written. It shows
; a very simple kind of forward chaining, made possible by sequencing
; primitive operations together in sequential execution. The chaining
; implements 'transitive closure', in that the recursive search extends
; the relation transitively.
;
(use-modules (opencog) (opencog exec))

; ----------
; Populate the AtomSpace with a tiny fragment of an upper ontology.
; The demo will be chaining these together, to reach conclusions
; about relationships.
(Inheritance (Concept "physical thing") (Concept "thing"))
(Inheritance (Concept "living thing") (Concept "physical thing"))
(Inheritance (Concept "animal") (Concept "living thing"))
(Inheritance (Concept "bilateria") (Concept "animal"))
(Inheritance (Concept "chordate") (Concept "bilateria"))
(Inheritance (Concept "vertebrate") (Concept "chordate"))
(Inheritance (Concept "mammal") (Concept "vertebrate"))
(Inheritance (Concept "human") (Concept "mammal"))
(Inheritance (Concept "Ben") (Concept "human"))

; ----------
; The PresentLink can be used to test if some clause is in the
; AtomSpace. The following just plugs "mammal" into "this" and
; "vertebrate" into "that", and then checks to see if there is
; an InheritanceLink of this kind in the AtomSpace. Running this
; should return true, or rather (SimpleTruthValue 1 1) which is
; printed in short-hand form as (stv 1 1)
(cog-evaluate!
	(Evaluation
		(Present (Inheritance (Variable "this") (Variable "that")))
		(List	(Concept "mammal") (Concept "vertebrate"))))

; We can check that "foobar" is not a vertebrate:
(cog-evaluate!
	(Evaluation
		(Present (Inheritance (Variable "this") (Variable "that")))
		(List	(Concept "foobar") (Concept "vertebrate"))))

; ----------
; Here's what we cannot do, or rather, should not do: we should not
; attempt to run
;
;    (cog-execute!
;        (Present (Inheritance (Concept "foo") (Concept "vertebrate"))))
;
; because doing so would have the side-effect of inserting the
; InheritanceLink relationship between "foo" and vertebrates directly
; into the AtomSpace. The PresentLink does not magically hold it's
; arguments outside of the AtomSpace: All Links, including the
; PresentLink, always insert their outgoing set into the AtomSpace.
;
; Thus, to avoid polluting the AtomSpace with the stuff that we wish to
; check for, variables are used. They isolate the form of the question
; being asked ("is something present in the AtomSpace?") from the
; specifics of what that question is being applied to.
;
; When the cog-evaluate! runs, it substitutes (beta-reduces) the
; ConceptNodes into the VariableNodes, and then evaluates the result.
; The evaluation is done outside of the AtomSpace, so that it is not
; polluted with junk. (More complex queries use a scratch AtomSpace to
; hold temporary results; this is invisible to the user. This query is
; simple enough that it does not need a scratch AtomSpace.)
;
; ----------
; Farther down in the demo, a fully recursive query will be defined,
; that will answer the question of whether something inherits from
; something else. To define that recursive query, we'll need to use the
; DefineLink, since, to call it (recursively) we need to attach a name
; to it. This is what DefineLink does: it attaches names to Atoms.
;
; The query will have to take two arguments: "this" and "that", as
; above. However, because it will have a more complex structure, it
; needs to be defined in terms of a LambdaLink, to specify the location
; of the arguments. LambdaLinks are the Atomese version of conventional
; lambdas: they are used to bind the variables appearing in an
; expression.
;
; The query is a predicate: that is, when run, it will return a
; true/false value. Thus, the query is defined to be a DefinedPredicate.
; This indicates to the system that it can be evaluated, and will result
; in a TruthValue. We make a point of this, because most AtomSpace
; contents are typically not predicates, and are not evaluatable or
; executable. Atomese is typed, in order to simplify reasoning over
; symbolic content.
;
; These three ideas are combined below. The predicate abstracts away
; (puts a wrapper around) the internal details. The InheritanceLink is
; now hidden in the guts of the Lambda, invisible to later uses.
;
(Define
	(DefinedPredicate "simple is-a relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Present (Inheritance (Variable "this") (Variable "that")))))

; Lets verify that this works as expected, that is, works as before:
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "simple is-a relation")
		(List	(Concept "mammal") (Concept "vertebrate"))))

; The same query also works without the intervening Define; one can
; stick the Lambda directly into place in the EvaluationLink. This
; is how the evaluation proceeds: the definition is expanded in place,
; and then the evaluation is run.
(cog-evaluate!
	(Evaluation
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Present (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "mammal") (Concept "vertebrate"))))

; ----------
; There is an explicit AbsentLink, as well. It's the opposite of the
; PresentLink.
(cog-evaluate!
	(Evaluation
		(Absent (Inheritance (Variable "this") (Variable "that")))
		(List	(Concept "foobar") (Concept "vertebrate"))))

; Of course, we could have said "not present"; the AbsentLink is not
; really needed for this demo; it is far more useful and powerful
; when it appears in patterns.
(cog-evaluate!
	(Evaluation
		(Not (Absent (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "mammal") (Concept "vertebrate"))))

; ----------
; Verifying a grand-parent/grand-child relationship requires moving to a
; different syntax. The query below requests that something in the
; middle is present in the AtomSpace. (If this seems opaque or
; confusing, please review the earlier demos that explain how searches
; are performed.)
(cog-evaluate!
	(Satisfaction
		(Present
			(Inheritance (Concept "human") (Variable "middle"))
			(Inheritance (Variable "middle") (Concept "vertebrate")))))

; The above works fine, and is reasonably readable. The only problem
; is that the two constants appear in the middle of the expression.
; These can be pulled out with a Lambda. It is a bit verbose, but
; it allows a generic predicate to be defined.
(Define
	(DefinedPredicate "grandparent relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Satisfaction
			(Variable "middle")
			(Present
				(Inheritance (Variable "this") (Variable "middle"))
				(Inheritance (Variable "middle") (Variable "that"))))))

; This new predicate can be used exactly the same way as the earlier
; one.  All the intervening details have been hidden.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "grandparent relation")
		(List	(Concept "foobar") (Concept "vertebrate"))))

; We can check that it indeed "skips a step" correctly.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "grandparent relation")
		(List	(Concept "human") (Concept "vertebrate"))))

; ----------
; What about the general recursive case? It needs to implement
; 'transitive closure'. This is the idea that given some relation
; R(x,y) (in this case, the InheritanceLink) that either one has
; R(a,b) is directly, immediately true for elements a,b or that
; there is a transitive chain
;
;     R(a,x) & R(x,y) & ... & R(z,b)
;
; for some intermediate elements x,y,...,z. The & here denotes logical
; 'and'; each of the R must be true.
;
; The above can be implemented programmatically by defining a recursive
; relation S(x,y) as follows:
;
;    S(x,y) := R(x,y) or (R(x,w) & S(w,y))
;
; The := symbol here is the definition of S. It is recursive in that the
; definition makes reference to itself. It just says that either S is R,
; or that we can peel off one level, and try again.
;
; ----------
; Philosophical digression:
;
; Converting the above to Atomese is sadly rather verbose. It is not at
; all compact. It's busy and verbose. If one wants to have simple,
; easy-to-read compact expressions, one should create a 'Domain-Specific
; Language' (DSL) on top of Atomese. Atomese is kind-of-like assembly
; code: it is not intended for human programmers, but for other algorithms
; to operate on. It needs to be easy for those other algorithms to work
; with; the side-effect is that it is verbose, and can be a bit tedious
; for humans.
;
; Attention! We have NOT created a DSL for recursive queries because the
; AtomSpace is meant to be a general system for holding arbitrary data,
; and should be usable in a broad variety of domains! Picking one
; particular syntax over another for representing a recursive query
; just perpetuates the problem of human-oriented programming languages.
; The prolog/datalog people will fight with the json/GraphQL people,
; who are unhappy with the SQL people. Let's not forget the probabilistic
; programming people, who reject all these approaches! The goal here is
; to avoid the domain-specific squabbles, and simply to provide tools to
; actually do things. It is up to the user to add a pretty DSL for this.
;
; ----------
; Diatribe aside, here is an annotation of the Atomese below.
; 1) Start with a definition: the name of the recursive function.
; 2) The lambda: it binds two variables, as before.
; 3) A SequentialOr. Evaluation stops as soon as one of the terms
;    returns true.
; 4) A direct check of inheritance. If this evaluates to true, then
;    we are done.
; 5) If not, then a SatisfactionLink, much as before.
; 6) The SatisfactionLink is looking to see if "this" is connected
;    to the middle.
; 7) But we also need the middle connected to a recursively long
;    chain. To get that, we refer to the recursive definition itself.
;    That definition takes two arguments. But which two arguments?
;    The PutLink explains exactly which two: the middle, and the
;    other endpoint. It "plugs things in" (it forms beta redexes.)
;    (Earlier demos explain PutLink. It's not complicated.)
;
(Define
	(DefinedPredicate "recursive relation")                 ;; Step 1.
	(Lambda
		(VariableList (Variable "this") (Variable "that"))   ;; Step 2.
		(SequentialOr                                        ;; Step 3.
			(Present
				(Inheritance (Variable "this") (Variable "that"))) ;; Step 4.
			(Satisfaction
				(Variable "middle")                            ;; Step 5.
				(And
					(Present                                    ;; Step 6.
						(Inheritance (Variable "this") (Variable "middle")))
					(Put                                        ;; Step 7.
						(DefinedPredicate "recursive relation")
						(List (Variable "middle") (Variable "that"))))))))

; Let's test it out. Does it work?
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "recursive relation")
		(List	(Concept "Ben") (Concept "animal"))))

; We can also verify that Ben isn't foobar'ed.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "recursive relation")
		(List	(Concept "Ben") (Concept "foobar"))))

; The predicate can be used to search the AtomSpace for all Atoms
; that obey the relationship.  Here, we ask for all the things that
; Ben might be. The query variable is "?inh", and we constrain it
; to be of type 'Concept, to limit the search. As before, the predicate
; takes two arguments; we have to plug the search variable into the
; right place, using PutLink to do the plugging-in.
;
; (Earlier demos explain MeetLink; it is a variant of GetLink, QueryLink
; and BindLink. The name refers to a 'lattice meet'. It returns the set
; of all things that satisfy a list of predicates. Here, we have only
; one predicate.)
(cog-execute!
	(Meet (TypedVariable (Variable "?inh") (Type 'Concept))
		(Put
			(DefinedPredicate "recursive relation")
			(List (Concept "Ben") (Variable "?inh")))))

; ----------
; The above demos all used the EvaluationLink, which, by definition,
; will always return a TruthValue.  By contrast, the ExecutionOutput
; link behaves similarly, except that it can return general Values or
; even Atoms. Of course, in this demo, it returns the same
; SimpleTruthValue as before, because that is what the PresentLink
; returns.
;
; Note some differences: we use cog-execute! here, not cog-evaluate!
(cog-execute!
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Present (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "mammal") (Concept "vertebrate"))))

; ExecutionOutput was designed to work with DefinedSchema, not
; DefinedPredicate. But otherwise, the same ideas apply. (Schemas
; can return Atoms; Predicates only return TruthValues.)
;
; ----------
; That's All Folks!  The End!
