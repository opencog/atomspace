;; Used by ExecutionOutputUTest::test_value

(use-modules (opencog) (opencog exec))

;; Grounds

(Implication
	(Schema "sc-1")
	(Concept "a"))

(Implication
	(Schema "sc-2")
	(Concept "b"))

(Implication
	(Schema "sc-3")
	(Concept "c"))

;; Query

(define (ret-handle A B) (Unordered A B))
(define (ret-link-value A B) (LinkValue A B))
(define (ret-truth-value A B) (SimpleTruthValue 0.6 0.3))
(define (ret-string-value A B) (StringValue (cog-name A) (cog-name B)))

(define (make-exec-query fun)
	(Query
		(VariableList
			(TypedVariable (Variable "$schema") (Type 'SchemaNode))
			(TypedVariable (Variable "$arg") (Type 'ConceptNode)))
		(Implication (Variable "$schema") (Variable "$arg"))
		(ExecutionOutput
			(GroundedSchema fun)
			(List (Variable "$schema") (Variable "$arg")))
		))

(define exec-query-handle (make-exec-query "scm: ret-handle"))
(define exec-query-link-value (make-exec-query "scm: ret-link-value"))
(define exec-query-truth-value (make-exec-query "scm: ret-truth-value"))
(define exec-query-string-value (make-exec-query "scm: ret-string-value"))
