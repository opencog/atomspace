#! /usr/bin/env guile
-s
!#
;
; count-pipeline-test.scm -- Verify that the count-pipeline demo works.
;
(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "count-pipeline-test")
(test-begin tname)

; ------------------------------------------------------------
; Pipeline Stage 1: Get all atoms
(PipeLink
	(Name "get-all-atoms")
	(Meet
		(Variable "$atom") ; vardecl
		(Variable "$atom") ; match anything, everything
	))

; ------------------------------------------------------------
; Pipeline Stage 2: Get type of each atom
(PipeLink
	(Name "get-types")
	(Filter
		(Rule
			(TypedVariable (Variable "$atom") (Type 'Atom)) ; vardecl
			(Variable "$atom") ; body - accept everything
			(TypeOf (DontExec (Variable "$atom")))) ; rewrite
		(Name "get-all-atoms")))

; ------------------------------------------------------------
; Pipeline Stage 3: Count each type occurrence
(Pipe
	(Name "count-types")
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(Variable "$typ") ; body - accept everything
			(IncrementValue (Variable "$typ") (Predicate "cnt") (Number 0 0 1)))
		(Name "get-types")))

; Run the counting pipeline for the first time.
; At this point, relatively few atoms exist in the AtomSpace.
(cog-execute! (Name "count-types"))

; ------------------------------------------------------------
; Pipeline Stage 4: De-duplicate types
(Pipe
	(Name "unique-types")
	(CollectionOf (TypeNode 'UnisetValue)
		(Name "get-types")))

; ------------------------------------------------------------
; Pipeline Stage 5: Define the sort order function
(DefineLink
	(DefinedPredicate "count-order")
	(Lambda
		(VariableList (Variable "left") (Variable "right"))
		(Not
			(LessThan
				(ElementOf (Number 2)
					(ValueOf (Variable "left") (Predicate "cnt")))
				(ElementOf (Number 2)
					(ValueOf (Variable "right") (Predicate "cnt")))))))

; Pipeline Stage 6: Create sorted collection
(Pipe
	(Name "sorted-types")
	(LinkSignature
		(TypeNode 'SortedValue)
		(DefinedPredicate "count-order")
		(Name "unique-types")))

; ------------------------------------------------------------
; Counter function - counts types and accumulates total count
; Returns the TypeNode, and as a side effect increments the global counter.
(define *type-count* 0)
(define *total-count* 0)

(define (type-counter NAME COUNT)
	(set! *type-count* (+ *type-count* 1))
	(set! *total-count* (+ *total-count* (inexact->exact (cog-value-ref COUNT 2))))
	NAME) ; Return the TypeNode

; Reset counters
(define (reset-counters)
	(set! *type-count* 0)
	(set! *total-count* 0))

; ------------------------------------------------------------
; Define the guarded counter - only counts types that have values
(define guarded-counter
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(And
				(Variable "$typ") ; body - accept everything
				(Equal            ; evaluatable guard
					(Type 'FloatValue)
					(TypeOf (ValueOf (Variable "$typ") (Predicate "cnt")))))

			(ExecutionOutput
				(GroundedSchema "scm:type-counter")
				(LinkSignature (Type 'LinkValue)
					(Variable "$typ")
					(ValueOf (Variable "$typ") (Predicate "cnt")))))
		(Name "sorted-types")))

; ------------------------------------------------------------
; TEST 1: First execution of guarded-counter
; At this point, only the basic pipeline atoms have been counted.
; Some TypeNodes created after count-types ran won't have counts.

(reset-counters)
(define first-result (cog-execute! guarded-counter))

; The result should be a LinkValue containing TypeNodes
(test-assert "first-result-is-link-value"
	(equal? 'LinkValue (cog-type first-result)))

; Should have roughly 10-20 types with counts (the exact number depends
; on how many atoms existed before the first count-types ran)
(test-assert "first-type-count-reasonable"
	(and (>= *type-count* 8) (<= *type-count* 25)))

; Save first run values for comparison
(define first-type-count *type-count*)
(define first-total-count *total-count*)

(format #t "First run: ~A types, total count ~A\n" first-type-count first-total-count)

; Verify that the total count is reasonable (should be at least as many
; atoms as types, since each type represents at least one atom)
(test-assert "first-total-count-reasonable"
	(>= first-total-count first-type-count))

; ------------------------------------------------------------
; TEST 2: Verify specific types have counts
; TypeNode should definitely have a count since TypeNodes are used heavily
(define type-type (Type 'TypeNode))
(define type-count-val (cog-value type-type (Predicate "cnt")))

(test-assert "type-node-has-count"
	(not (equal? #f type-count-val)))

(test-assert "type-node-count-positive"
	(> (cog-value-ref type-count-val 2) 0))

; ------------------------------------------------------------
; TEST 3: Run count-types again - this will add to existing counts
; and count new types that were created (like AndLink from the guard)

(cog-execute! (Name "count-types"))

(reset-counters)
(define second-result (cog-execute! guarded-counter))

(define second-type-count *type-count*)
(define second-total-count *total-count*)

(format #t "Second run: ~A types, total count ~A\n" second-type-count second-total-count)

; After the second run, more types should have counts
; (types created between first and second count-types runs)
(test-assert "second-type-count-at-least-first"
	(>= second-type-count first-type-count))

; The total count should be significantly higher after the second run
; because each type's count was incremented again
(test-assert "second-total-count-higher"
	(> second-total-count first-total-count))

; The increase should be substantial - at least double the original count
; because each atom gets counted again, plus new atoms
(test-assert "second-count-substantially-higher"
	(>= second-total-count (* 2 first-total-count)))

; ------------------------------------------------------------
; TEST 4: Verify counts are cumulative
; The TypeNode count should have increased from the second run

(define type-count-val-2 (cog-value type-type (Predicate "cnt")))
(define type-count-1 (cog-value-ref type-count-val 2))
(define type-count-2 (cog-value-ref type-count-val-2 2))

(format #t "TypeNode count: first=~A second=~A\n" type-count-1 type-count-2)

(test-assert "type-node-count-increased"
	(> type-count-2 type-count-1))

; ------------------------------------------------------------
; TEST 5: Run count-types a third time and verify continued accumulation

(cog-execute! (Name "count-types"))

(reset-counters)
(define third-result (cog-execute! guarded-counter))

(define third-type-count *type-count*)
(define third-total-count *total-count*)

(format #t "Third run: ~A types, total count ~A\n" third-type-count third-total-count)

; Total count should continue to increase
(test-assert "third-total-count-higher-than-second"
	(> third-total-count second-total-count))

; Verify TypeNode count increased again
(define type-count-val-3 (cog-value type-type (Predicate "cnt")))
(define type-count-3 (cog-value-ref type-count-val-3 2))

(test-assert "type-node-count-increased-again"
	(> type-count-3 type-count-2))

(format #t "TypeNode count progression: ~A -> ~A -> ~A\n"
	type-count-1 type-count-2 type-count-3)

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
