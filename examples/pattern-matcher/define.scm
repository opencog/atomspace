;
; define.scm -- Using DefineLink to give names to things.
;
(use-modules (opencog) (opencog exec))

;; Some data to populate the AtomSpace.
(Inheritance
	(Concept "battery")
	(Concept "electrical device"))

(Inheritance
	(Concept "transistor")
	(Concept "electrical device"))

(Edge
	(Predicate "PartOf")
	(List
		(Concept "battery")
		(Variable "car")))

(Edge
	(Predicate "PartOf")
	(List
		(Concept "transistor")
		(Variable "phone")))

(Edge
	(Predicate "PartOf")
	(List
		(Concept "windshield")
		(Variable "car")))

;; Define the concept of electrical parts of things.
;; Both clauses must be present, for this to evaluate to true!
(DefineLink
	(DefinedPredicate "Electrical Part Of")
	(Present
		(Inheritance
			(Variable "$x")
			(Concept "electrical device"))
		(Edge
			(Predicate "PartOf")
			(List
				(Variable "$x")
				(Variable "$y")))))

;; Define a pattern to find the electrical parts of things.
;; Variables are automatically extracted from the definition.
(define get-elect
	(Meet (DefinedPredicate "Electrical Part Of")))

;; Search the AtomSpace for electrical things.
(cog-execute! get-elect)

;; ==================================================================
;;
;; Patterns can also be assembled out of multiple Defines,
;; and mixed with other kinds of clauses.

(DefineLink
	(DefinedPredicate "Electrical Thing")
	(Inheritance
		(Variable "$x")
		(Concept "electrical device")))

(DefineLink
	(DefinedPredicate "Part-whole Relation")
	(Edge
		(Predicate "PartOf")
		(List
			(Variable "$x")
			(Variable "$y"))))

;; Define a predicate to "do things" (in this case, to print)
;; Be sure to return a truth value!
(define cnt 0)
(define (do-stuff atom)
	(set! cnt (+ cnt 1))
	(format #t "At count ~a found this part: ~a \n" cnt atom)
	#t)

(DefineLink
	(DefinedPredicate "Counter Printer")
	(Evaluation (GroundedPredicate "scm: do-stuff")
		(List (Variable "$x"))))

;; Assemble a pattern out of the parts above. Notice that the variables
;; in each of the different defines are joined together.
(define get-electrical-parts
	(Meet
		(And
			(DefinedPredicate "Electrical Thing")
			(DefinedPredicate "Part-whole Relation")
			(DefinedPredicate "Counter Printer")
)))

(cog-execute! get-electrical-parts)
