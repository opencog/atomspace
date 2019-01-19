;
; knowledge.scm - Representing data with Predicates and Evaluations
;
; The atomspace is primarily a knowledge-represention database 
; (a "knowledgebase"). That is, you have a collection of statements:
; some "semantic triples" or maybe some "ontology". A "semantic netowrk"
; or some "frames" or some "rules" or maybe even some "axioms".
;
; There is a tremendous variety of ways that the above can be
; represented in the Atomspace. This example reviews a few of the
; most basic, most common forms.
;

(use-modules (opencog))

; Some typical natural-language dependency data.  Consider a dependency
; parse of the sentence "Susan makes pottery". Here, "makes" is the
; verb, and "pottery" is the object of the verb. Thuse, the dependency
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
