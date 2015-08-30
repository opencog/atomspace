;
; define.scm
;
; Demonstrate the use of DefineLink to give names to things.
;
; XXX caution: under construction, unstable, known buggy.
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
;; Both clauses must be present, for this to evaluate to true!
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

;; Define a pattern to find the electrical parts of things.
;; Variables are automatically extracted from the definition.
(define get-elect
	(GetLink (DefinedPredicateNode "Electrical Part Of")))

;; Search the atomspace for electrical things.
(cog-execute! get-elect)

;; ==================================================================
;;
;; Patterns can also be assembled out of multiple DefineLinks,
;; and mixed with other kinds of clauses.

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

;; Define a predicate to "do things" (in this case, to print)
;; Be sure to return a truth value!
(define cnt 0)
(define (do-stuff atom)
	(set! cnt (+ cnt 1))
	(format #t "At count ~a found this part: ~a \n" cnt atom)
	(stv 1 1))

;; Assemble a pattern out of the parts above. Notice that the variables
;; in each of the different defines are joined together.
(define get-electrical-parts
	(GetLink
		(AndLink
			(DefinedPredicateNode "Electrical Thing")
			(DefinedPredicateNode "Part-whole Relation")
			(EvaluationLink (GroundedPredicateNode "scm: do-stuff")
				(ListLink (VariableNode "$x")))
)))

(cog-execute! get-electrical-parts)
