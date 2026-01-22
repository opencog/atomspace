#! /usr/bin/env guile
-s
!#
;
; count-pipeline-test.scm -- Verify that the count-pipeline demo works.
;
(use-modules (opencog))
(use-modules (opencog test-runner))
(use-modules (srfi srfi-1)) ; for fold

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
(Trigger (Name "count-types"))

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
(define first-result (Trigger guarded-counter))

; The result should be a LinkValue containing TypeNodes
(test-assert "first-result-is-link-value"
	(equal? 'LinkValue (cog-type first-result)))

; Should have exactly 13 types with counts
(test-assert "first-type-count" (= *type-count* 13))
(test-assert "first-total-count" (= *total-count* 22))

(format #t "First run: ~A types, total count ~A\n"
	*type-count* *total-count*)

; ------------------------------------------------------------
; TEST 2: Verify specific types have counts
; TypeNode should definitely have a count since TypeNodes are used heavily
(define type-count-1 (cog-value-ref (Type 'TypeNode) (Predicate "cnt") 2))

(test-assert "type-node-count-is-two" (= type-count-1 2))

; ------------------------------------------------------------
; TEST 3: Run count-types again - this will add to existing counts
; and count new types that were created (like AndLink from the guard)

(Trigger (Name "count-types"))

(reset-counters)
(Trigger guarded-counter)

(format #t "Second run: ~A types, total count ~A\n"
	*type-count* *total-count*)

; After the second run, more types should have counts
; Twenty-seven, in fact.
(test-assert "second-type-count" (= *type-count* 27))

; Oh what the heck. It has to be 102, eactly.
(test-assert "second-total-count" (= *total-count* 102))

; ------------------------------------------------------------
; TEST 4: Verify counts are cumulative
; The TypeNode count should have increased from the second run

(define type-count-2 (cog-value-ref (Type 'TypeNode) (Predicate "cnt") 2))
(test-assert "type-node-count" (= 34 type-count-2))

; ------------------------------------------------------------
; TEST 5: Run count-types a third time and verify continued accumulation

(Trigger (Name "count-types"))

(reset-counters)
(Trigger guarded-counter)

(format #t "Third run: ~A types, total count ~A\n"
	*type-count* *total-count*)

; Total count should continue to increase
(test-assert "third-total-count" (= 182 *total-count*))

; Verify TypeNode count increased again
(define type-count-3 (cog-value-ref (Type 'TypeNode) (Predicate "cnt") 2))
(test-assert "type-node-count-3" (= 66 type-count-3))

(format #t "TypeNode count progression: ~A -> ~A -> ~A\n"
	type-count-1 type-count-2 type-count-3)

; ------------------------------------------------------------
; TEST 6: Count using pure Atomese.

(Pipe
	(Name "grand-total")
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(And
				(Variable "$typ") ; body - accept everything
				(Equal				; evaluatable guard
					(Type 'FloatValue)
					(TypeOf (ValueOf (Variable "$typ") (Predicate "cnt")))))
			(IncrementValue
				(AnyNode "totals")
				(Predicate "tot-counts")
				(ValueOf (Variable "$typ") (Predicate "cnt")))
			(IncrementValue
				(AnyNode "totals")
				(Predicate "tot-types")
				(Number 0 0 1)))
		(Name "unique-types")))

(Trigger (Name "grand-total"))

(define tot-types (cog-value-ref (Any "totals") (Predicate "tot-types") 2))
(test-assert "tot-types" (= 27 tot-types))

(define tot-counts (cog-value-ref (Any "totals") (Predicate "tot-counts") 2))
(test-assert "tot-counts" (= 182 tot-counts))

; ------------------------------------------------------------
; TEST 7: Count derefernced value

(Pipe
	(Name "deref grand-total")
	(Filter
		(Rule
			(VariableList
				(TypedVariable (Variable "$typ") (Type 'Type))
				(TypedVariable (Variable "$count") (Type 'FloatValue))) ; vardecl
			(And
				(Variable "$typ") ; body - accept everything
				(Equal				; evaluatable guard
					(Variable "$count")
					(TypeOf (ValueOf (Variable "$typ") (Predicate "cnt")))))
			(IncrementValue
				(AnyNode "deref totals")
				(Predicate "tot-counts")
				(Variable "$count"))
			(IncrementValue
				(AnyNode "deref totals")
				(Predicate "tot-types")
				(Number 0 0 1)))
		(Name "unique-types")))

;;; Stub out, for now. Using the Equal predicate, like the above,
;;; seems like a cool idea ... kind-of. But I want to draw the line
;;; at this kind of ... nonsense. The Equal above starts to look a
;;; whole lot like olde-fashioned procedural programming, and we know
;;; what lies down that path. I have no desire to create a Sinclair
;;; Z80 in Minecraft. We're not emulating to emulate badly and
;;; inefficiently. ... at least not yet ... :-/
;;; (Trigger (Name "deref grand-total"))
;;;
;;; (define tot-types (cog-value-ref (Any "deref totals") (Predicate "tot-types") 2))
;;; (test-assert "deref tot-types" (= 27 tot-types))
;;;
;;; (define tot-counts (cog-value-ref (Any "deref totals") (Predicate "tot-counts") 2))
;;; (test-assert "deref tot-counts" (= 182 tot-counts))

; ------------------------------------------------------------
; TEST 8: Verify PureExec prevents double-counting
;
; This tests the full type-counts pipeline from atomspace-viz.
; The key feature is that it uses PureExec to run the counting in
; a fresh (scratch) AtomSpace, which prevents double-counting.
; Running the pipeline multiple times should produce identical results.

; Define the comparison predicate for sorting types by count.
(DefineLink
	(DefinedPredicate "viz-count-order")
	(Lambda
		(VariableList (Variable "left") (Variable "right"))
		(Not
			(LessThan
				(ElementOf (Number 2)
					(ValueOf (Variable "left") (Predicate "vcnt")))
				(ElementOf (Number 2)
					(ValueOf (Variable "right") (Predicate "vcnt")))))))

; The type-counting pipeline that uses PureExec to prevent double-counting.
; This is a copy of the pipeline from atomspace-viz/analytics/count-types.scm
(PipeLink
	(Name "viz-type-counts")
	(PureExec
		(Filter
			(Rule
				(TypedVariable (Variable "$typ") (Type 'Type))
				; Guard: only accept types that have a count attached
				(And
					(Present (Variable "$typ"))
					(Equal
						(Type 'FloatValue)
						(TypeOf (ValueOf (Variable "$typ") (Predicate "vcnt")))))
				; Output: table row with type and count
				(LinkSignature (Type 'LinkValue)
					(Variable "$typ")
					(ValueOf (Variable "$typ") (Predicate "vcnt"))))
			; Input: sorted unique types
			(LinkSignature
				(TypeNode 'SortedValue)
				(DefinedPredicate "viz-count-order")
				; Input: unique types (deduplicated)
				(CollectionOf (TypeNode 'UnisetValue)
					; Input: types extracted from atoms, with counts attached
					(Filter
						(Rule
							(TypedVariable (Variable "$typ") (Type 'Type))
							(Variable "$typ")
							; Increment the count on this TypeNode, return TypeNode
							(IncrementValueOn (Variable "$typ") (Predicate "vcnt") (Number 0 0 1)))
						; Input: types of all atoms
						(Filter
							(Rule
								(TypedVariable (Variable "$atom") (Type 'Atom))
								(Variable "$atom")
								(TypeOf (DontExec (Variable "$atom"))))
							; Input: all atoms in the base AtomSpace
							(PureExec
								(AtomSpaceOf (Link))
								(Meet
									(Variable "$atom")
									(Variable "$atom"))))))))))

; Helper to sum all counts in the result
(define (sum-counts result)
	(fold (lambda (row acc)
		(+ acc (inexact->exact (cog-value-ref (cog-value-ref row 1) 2))))
		0
		(cog-value->list result)))

; Helper to count rows in result
(define (count-rows result)
	(length (cog-value->list result)))

; Run the pipeline three times and collect results
(define viz-result-1 (Trigger (Name "viz-type-counts")))
(define viz-rows-1 (count-rows viz-result-1))
(define viz-sum-1 (sum-counts viz-result-1))

(format #t "PureExec run 1: ~A types, total ~A\n" viz-rows-1 viz-sum-1)

(define viz-result-2 (Trigger (Name "viz-type-counts")))
(define viz-rows-2 (count-rows viz-result-2))
(define viz-sum-2 (sum-counts viz-result-2))

(format #t "PureExec run 2: ~A types, total ~A\n" viz-rows-2 viz-sum-2)

(define viz-result-3 (Trigger (Name "viz-type-counts")))
(define viz-rows-3 (count-rows viz-result-3))
(define viz-sum-3 (sum-counts viz-result-3))

(format #t "PureExec run 3: ~A types, total ~A\n" viz-rows-3 viz-sum-3)

; The key test: all three runs should produce identical counts
; If double-counting occurs, the counts would increase each time
(test-assert "pureexec-consistent-row-count-1-2" (= viz-rows-1 viz-rows-2))
(test-assert "pureexec-consistent-row-count-2-3" (= viz-rows-2 viz-rows-3))
(test-assert "pureexec-consistent-total-1-2" (= viz-sum-1 viz-sum-2))
(test-assert "pureexec-consistent-total-2-3" (= viz-sum-2 viz-sum-3))

(format #t "PureExec consistency verified: ~A types, ~A total (all 3 runs identical)\n"
	viz-rows-1 viz-sum-1)

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
