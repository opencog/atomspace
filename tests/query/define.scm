;
; define.scm
;
; Demonstrate the use of DefineLink to give names to things.
;
(use-modules (opencog))
(use-modules (opencog query))
(use-modules (opencog exec))

; (load-from-path "utilities.scm")

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
		(DefinedPredicateNode "Electrical Part Of")
		(PresentLink
			(InheritanceLink
				(VariableNode "$x")
				(ConceptNode "electrical device"))
			(EvaluationLink
				(PredicateNode "PartOf")
				(ListLink
					(VariableNode "$x")
					(VariableNode "$y"))))))

;; Define a pattern to find the electrical parts of things
;; At this time, an explicit variable declaration has to be done;
;; this needs to be fixed...
(define get-elect
	(GetLink 
		(VariableList (VariableNode "$x") (VariableNode "$y"))
		(DefinedPredicateNode "Electrical Part Of")))

;; Search the atomspace for electrical things.
;; (cog-execute! (get-elect))
