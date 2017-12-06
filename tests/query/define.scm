;
; define.scm
;
; Demonstrate the use of DefineLink to give names to things.
;
(use-modules (opencog))
(use-modules (opencog exec))

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
				(VariableNode "$y")))))

; A version of above with lambda-bound variables.
(DefineLink
	(DefinedPredicateNode "Elect-Part bound")
	(LambdaLink
		(VariableList (VariableNode "$x") (VariableNode "$y"))
		(PresentLink
			(InheritanceLink
				(VariableNode "$x")
				(ConceptNode "electrical device"))
			(EvaluationLink
				(PredicateNode "PartOf")
				(ListLink
					(VariableNode "$x")
					(VariableNode "$y"))))))

; Split up into parts...
(DefineLink
   (DefinedPredicateNode "Electrical Thing")
   (InheritanceLink
      (VariableNode "$x")
      (ConceptNode "electrical device")))

(DefineLink
   (DefinedPredicateNode "Part-whole Relation")
   (EvaluationLink
      (PredicateNode "PartOf")
      (ListLink
         (VariableNode "$x")
         (VariableNode "$y"))))

;; Define a pattern to find the electrical parts of things
;; At this time, an explicit variable declaration has to be done;
;; this needs to be fixed...
(define get-elect
	(GetLink (DefinedPredicateNode "Electrical Part Of")))

(define get-elect-bound
	(GetLink (DefinedPredicateNode "Elect-Part bound")))

(define get-parts
	(GetLink
		(AndLink
			(DefinedPredicateNode "Electrical Thing")
			(DefinedPredicateNode "Part-whole Relation"))))

;; Search the atomspace for electrical things.
;; (cog-execute! (get-elect))
