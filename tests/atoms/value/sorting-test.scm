;
; sorting-test.scm -- Test SortedStream sorting by size
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)

; Helper function to get numeric size of an atom
(define (get-size atom)
	(car (cog-value->list (cog-execute! (SizeOf atom)))))

; ---------------------------------------------------------------
; Test greater-or-equal ordering (descending by size)
;
(define tname "sorting-greater-or-equal")
(test-begin tname)

(define greater-or-equal-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Or
			(GreaterThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right")))
			(Equal
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

(define item-list
	(OrderedLink
		(Item "a")
		(Item "b")
		(Edge
			(Predicate "relation")
			(List (Item "c") (Item "d")))
		(Item "e")
		(Item "f")
		(Link
			(Item "p")
			(Predicate "q")
			(TagNode "z"))
		(Edge
			(Predicate "relation")
			(List (Item "g") (Item "h")))
	))

(define ge-stream (SortedStream greater-or-equal-relation item-list))

(define item1 (car (cog-value->list ge-stream)))
(format #t "Item 1: ~A (size ~A)\n" item1 (get-size item1))
(test-assert "ge-item1-size-3" (equal? (get-size item1) 3.0))

(define item2 (car (cog-value->list ge-stream)))
(format #t "Item 2: ~A (size ~A)\n" item2 (get-size item2))
(test-assert "ge-item2-size-2" (equal? (get-size item2) 2.0))

(define item3 (car (cog-value->list ge-stream)))
(format #t "Item 3: ~A (size ~A)\n" item3 (get-size item3))
(test-assert "ge-item3-size-2" (equal? (get-size item3) 2.0))

(define item4 (car (cog-value->list ge-stream)))
(format #t "Item 4: ~A (size ~A)\n" item4 (get-size item4))
(test-assert "ge-item4-size-1" (equal? (get-size item4) 1.0))

(define item5 (car (cog-value->list ge-stream)))
(format #t "Item 5: ~A (size ~A)\n" item5 (get-size item5))
(test-assert "ge-item5-size-1" (equal? (get-size item5) 1.0))

(define item6 (car (cog-value->list ge-stream)))
(format #t "Item 6: ~A (size ~A)\n" item6 (get-size item6))
(test-assert "ge-item6-size-1" (equal? (get-size item6) 1.0))

(define item7 (car (cog-value->list ge-stream)))
(format #t "Item 7: ~A (size ~A)\n" item7 (get-size item7))
(test-assert "ge-item7-size-1" (equal? (get-size item7) 1.0))

(define end-of-stream (cog-value->list ge-stream))
(format #t "End of stream: ~A\n" end-of-stream)
(test-assert "ge-eos" (equal? end-of-stream '()))

(test-end tname)

; ---------------------------------------------------------------
; Test not-less-than ordering (descending by size)
;
(define tname-nlt "sorting-not-less-than")
(test-begin tname-nlt)

(define not-less-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Not
			(LessThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

(define nlt-stream (SortedStream not-less-relation item-list))

(define nlt-item1 (car (cog-value->list nlt-stream)))
(format #t "NLT Item 1: ~A (size ~A)\n" nlt-item1 (get-size nlt-item1))
(test-assert "nlt-item1-size-3" (equal? (get-size nlt-item1) 3.0))

(define nlt-item2 (car (cog-value->list nlt-stream)))
(format #t "NLT Item 2: ~A (size ~A)\n" nlt-item2 (get-size nlt-item2))
(test-assert "nlt-item2-size-2" (equal? (get-size nlt-item2) 2.0))

(define nlt-item3 (car (cog-value->list nlt-stream)))
(format #t "NLT Item 3: ~A (size ~A)\n" nlt-item3 (get-size nlt-item3))
(test-assert "nlt-item3-size-2" (equal? (get-size nlt-item3) 2.0))

(define nlt-item4 (car (cog-value->list nlt-stream)))
(format #t "NLT Item 4: ~A (size ~A)\n" nlt-item4 (get-size nlt-item4))
(test-assert "nlt-item4-size-1" (equal? (get-size nlt-item4) 1.0))

(test-end tname-nlt)

; ---------------------------------------------------------------
; Test not-greater-than ordering (ascending by size)
;
(define tname-ngt "sorting-not-greater-than")
(test-begin tname-ngt)

(define not-greater-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Not
			(GreaterThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

(define ngt-stream (SortedStream not-greater-relation item-list))

(define ngt-item1 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 1: ~A (size ~A)\n" ngt-item1 (get-size ngt-item1))
(test-assert "ngt-item1-size-1" (equal? (get-size ngt-item1) 1.0))

(define ngt-item2 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 2: ~A (size ~A)\n" ngt-item2 (get-size ngt-item2))
(test-assert "ngt-item2-size-1" (equal? (get-size ngt-item2) 1.0))

(define ngt-item3 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 3: ~A (size ~A)\n" ngt-item3 (get-size ngt-item3))
(test-assert "ngt-item3-size-1" (equal? (get-size ngt-item3) 1.0))

(define ngt-item4 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 4: ~A (size ~A)\n" ngt-item4 (get-size ngt-item4))
(test-assert "ngt-item4-size-1" (equal? (get-size ngt-item4) 1.0))

(define ngt-item5 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 5: ~A (size ~A)\n" ngt-item5 (get-size ngt-item5))
(test-assert "ngt-item5-size-2" (equal? (get-size ngt-item5) 2.0))

(define ngt-item6 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 6: ~A (size ~A)\n" ngt-item6 (get-size ngt-item6))
(test-assert "ngt-item6-size-2" (equal? (get-size ngt-item6) 2.0))

(define ngt-item7 (car (cog-value->list ngt-stream)))
(format #t "NGT Item 7: ~A (size ~A)\n" ngt-item7 (get-size ngt-item7))
(test-assert "ngt-item7-size-3" (equal? (get-size ngt-item7) 3.0))

(test-end tname-ngt)

; ---------------------------------------------------------------
; Test deduplication with strict greater-than
;
(define tname-gt "sorting-deduplication")
(test-begin tname-gt)

(define greater-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(GreaterThan
			(SizeOf (Variable "$left"))
			(SizeOf (Variable "$right")))))

(define gt-stream (SortedStream greater-relation item-list))

(define gt-item1 (car (cog-value->list gt-stream)))
(format #t "GT Item 1: ~A (size ~A)\n" gt-item1 (get-size gt-item1))
(test-assert "gt-item1-size-3" (equal? (get-size gt-item1) 3.0))

(define gt-item2 (car (cog-value->list gt-stream)))
(format #t "GT Item 2: ~A (size ~A)\n" gt-item2 (get-size gt-item2))
(test-assert "gt-item2-size-2" (equal? (get-size gt-item2) 2.0))

(define gt-item3 (car (cog-value->list gt-stream)))
(format #t "GT Item 3: ~A (size ~A)\n" gt-item3 (get-size gt-item3))
(test-assert "gt-item3-size-1" (equal? (get-size gt-item3) 1.0))

(define gt-eos (cog-value->list gt-stream))
(format #t "GT End of stream: ~A\n" gt-eos)
(test-assert "gt-eos" (equal? gt-eos '()))

(test-end tname-gt)

; ---------------------------------------------------------------
; Test LinkSignature constructor
;
(define tname-linksig "sorting-linksig")
(test-begin tname-linksig)

(define link-sig
	(LinkSignature (Type 'SortedStream) not-less-relation item-list))

(define cnlt (cog-execute! link-sig))

(define ls-item1 (car (cog-value->list cnlt)))
(format #t "LinkSig Item 1: ~A (size ~A)\n" ls-item1 (get-size ls-item1))
(test-assert "ls-item1-size-3" (equal? (get-size ls-item1) 3.0))

(define ls-item2 (car (cog-value->list cnlt)))
(format #t "LinkSig Item 2: ~A (size ~A)\n" ls-item2 (get-size ls-item2))
(test-assert "ls-item2-size-2" (equal? (get-size ls-item2) 2.0))

(define ls-item3 (car (cog-value->list cnlt)))
(format #t "LinkSig Item 3: ~A (size ~A)\n" ls-item3 (get-size ls-item3))
(test-assert "ls-item3-size-2" (equal? (get-size ls-item3) 2.0))

(test-end tname-linksig)

(opencog-test-end)
