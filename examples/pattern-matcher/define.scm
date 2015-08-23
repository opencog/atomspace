;
; define.scm
;
; Demonstrate the use of DefineLink to give names to things.
;
(use-modules (opencog))
(use-modules (opencog query))

(load-from-path "utilities.scm")

;; Some data to populate the atomspace.
(InheritanceLink
	(ConceptNode "battery")
	(ConceptNode "electrical device"))

(InheritanceLink
	(ConceptNode "transistor")
	(ConceptNode "electrical device"))

(EvaluationLink
	(PredicateNode "PartOf")
	(ListLink
		(ConceptNode "battery")
		(VariableNode "car")))

(EvaluationLink
	(PredicateNode "PartOf")
	(ListLink
		(ConceptNode "transistor")
		(VariableNode "phone")))

(EvaluationLink
	(PredicateNode "PartOf")
	(ListLink
		(ConceptNode "windsheild")
		(VariableNode "car")))

;; Define the concept of electrical parts of things.
(define elect-parts
	(DefineLink
		(PredicateNode "Electrical Part Of")
		(AndLink
			(InheritanceLink
				(VariableNode "$x")
				(ConceptNode "electrical device"))
			(EvaluationLink
				(PredicateNode "PartOf")
				(ListLink
					(VariableNode "$x")
					(VariableNode "$y"))))))

;; Define a pattern to find the electrical parts of things
(define get-elect
	(GetLink elect-parts))

;; Search the atomspace for electrical things.
(cog-execute! get-elect)
