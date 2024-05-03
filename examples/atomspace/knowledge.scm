;
; knowledge.scm - Representing data with Predicates and Evaluations
;
; The atomspace is primarily a knowledge-representation database
; (a "knowledgebase"). That is, you have a collection of statements:
; some "semantic triples" or maybe some "ontology". A "semantic network"
; or some "frames" or some "rules" or maybe even some "axioms".
;
; There is a tremendous variety of ways that the above can be
; represented in the Atomspace. This example reviews a few of the
; most basic, most common forms.
;

(use-modules (opencog))

; Some typical natural-language dependency data.  Consider a dependency
; parse of the sentence "Susan makes pottery". Here, "makes" is the
; verb, and "pottery" is the object of the verb. Thus, the dependency
; is "_obj(make, pottery)". This can be represented as
;
(Evaluation
	(Predicate "_obj")
	(ListLink
		(Concept "make")
		(Concept "pottery")))

; There is no need to write this on five lines;
; you can write it on just one:
;
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "pottery")))

; "Evaluation" is the same type as "EvaluationLink",
; "Predicate" is the same type as "PredicateNode",
; "Concept" is the same type as "ConceptNode".
; Thus you can also write:

(EvaluationLink
	(PredicateNode "_obj")
	(ListLink
		(ConceptNode "make")
		(ConceptNode "pottery")))

; Nodes always have string names.
; Links are always collections of Nodes or of other Links.
;
; --------------------------------------------------------
; Schemas.
;
; The above example is "free-form".  There are no particular rules for
; how you can put data into the knowledgebase. This makes it quite
; unlike SQL, where you MUST declare a table, before putting data into
; it. Thus, Atomese is a bit more like ProLog: you can assert facts,
; without specifying any kind of template or schema in advance.
;
; However, some kinds of data problems do want a strict structuring
; of data. This can be achieved by using the TypedAtomLink. Using types
; in the AtomSpace is an advanced topic, but some flavor of this can
; be given here.
;
; Suppose you wanted to say that the "_obj" predicate must always take
; only two arguments, no more and no less, and that these must always
; be ConceptNodes. You can declare this by writing the following:

(Signature
	(Evaluation
		(Predicate "_obj")
		(ListLink
			(Type "ConceptNode")
			(Type "ConceptNode"))))

; The above declares a "signature" with the standard comp-sci meaning:
;     https://en.wikipedia.org/wiki/Type_signature
; aka the standard meaning in mathematical logic:
;     https://en.wikipedia.org/wiki/Signature_(logic)
;
; To use it, it is useful to attach a name to it, so that it can be
; referenced by name:

(TypedAtom
	(DefinedType "my obj dependency relation")
	(Signature
		(Evaluation
			(Predicate "_obj")
			(ListLink
				(Type "ConceptNode")
				(Type "ConceptNode")))))

; The above can be used during type-checking, to verify that certain
; data structures are in the expected format. See
;     https://wiki.opencog.org/w/SignatureLink
; and
;     https://wiki.opencog.org/w/TypedAtomLink
; for more info. Again -- signatures are an advanced topic; you can
; ignore this for now. Just know that they exist.
