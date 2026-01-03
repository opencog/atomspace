;
; sorted-value.scm -- Sorting Atoms and Values
;
; ContainerValues provide thread-safe, multi-reader, multi-writer
; dynamically-varying buffers for Values. The most basic container
; is the QueueValue; it provides a equential first-in, first-out FIFO.
; The UnisetValue is a deduplicating; it holds at most only once
; instance of an item (Atom or Value).
;
; The SortedValue allows a custom order relation to be specifed,
; and it holds it's contents in that order.  When items are removed,
; they come from the head of the order; when added, they are added in
; appropriate sort order.
;
; As a buffer, the SortedValue is not really intended for sorting
; static lists, but rather for sorting items arriving in a stream.
; The example below will (unfortunately) use a static list for the
; demo; mostly to keep the demo as simple as reasonable.
;
(use-modules (opencog))

: Demoing a running stream is difficult, and so the demo below will
; demonstrate sorting on a static list. This has little overall impact.
;
; The dataset to be sorted will consist of a collection of Atoms of
; varying sizes. The sort relation will examine the sizes, and order
; accordingly. Several variants of the sort relation are demoed:
; ascending, descending and "deduplicating". The deduplicating order
; is curious: it only admits one exemplar of a given size in the stream.
; Unlucky Atoms that happen to be of the same size, but are otherwise
; different, are discarded.
;
; The ordering relation can be any function that takes two arguments,
; and return a crisp ture/false BoolValue. In this wxample, the relation
; will be a LambdaLink binding two variables: left and right. These can
; then be used in arbitrarily complicated expressions in the lambda
; body. For the demo, the SizeOfLink provides a numerical value for the
; size of an Atom; the GreaterThanLink, LessThanLink, EqualLink and the
; boolean ops AndLin, OrLink can be combined.
;
(Define
	(DefinedPredicate "greater-or-equal")
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Or
			(GreaterThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right")))
			(Equal
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

; Create a list of items of varying sizes.
(PipeLink
	(Name "item-list")
	(OrderedLink
		(Item "a")       ; size 1
		(Item "b")       ; size 1
		(Edge            ; size 2 - the Predicate and the List
			(Predicate "relation")
			(List (Item "c") (Item "d")))
		(Item "e")       ; size 1
		(Item "f")       ; size 1
		(Link            ; size 3 - p,q and z: the largest in the bunch
			(Item "p")
			(Predicate "q")
			(TagNode "z"))
		(Edge            ; size 2
			(Predicate "relation")
			(List (Item "g") (Item "h")))
	))

; Construct the Value
(define ge-value
	(SortedValue
		(DefinedPredicate "greater-or-equal")
		(Name "item-list")))

; Access the Value in one big gulp: get the whole thing.
(cog-value->list ge-value)

; The largest came first. Note that this is not a "stable sort": some
; of the atoms that are later in the original list, get sorted earlier.
; This is an artifact of the implementation: it is using the C++
; std::set<> to hold the items; the std::set<> is usually implemented
; as an RB-tree, and makes no stability guarantees. Of course, this
; means that results will depend on the OS, the compiler and the c++
; library.

; --------------------------------------------------------
; Numerical (total) orders have the usual symmetries:
; Greater-or-equal is the same as not-less-than
(Define
	(DefinedPredicate "not-less")
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Not
			(LessThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

; Should work as before.
(define nlt-sort
	(SortedValue
		(DefinedPredicate "not-less")
		(Name "item-list")))

; Print the sorted list.
(cog-value->list nlt-sort)

; --------------------------------------------------------
; Make sure this is not a happy accident, and reverse the order.
(Define
	(DefinedPredicate "not-greater")
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Not
			(GreaterThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

; This time, the smallest come first.
(define ngt-sort
	(SortedValue
		(DefinedPredicate "not-greater")
		(Name "item-list")))

; Print the result
(cog-value->list ngt-sort)

; --------------------------------------------------------
; Now for fun and games: deduplication.
(Define
	(DefinedPredicate "greater-dedupe")
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(GreaterThan
			(SizeOf (Variable "$left"))
			(SizeOf (Variable "$right")))))

; The result here is surprising: only three items, of size 3,2 and 1
; So, indeed, in the proper greater-than ordering; but where did the
; rest go? The answer is provided by the vagaries of sorting when
; equality is deduced from the relation. Explicitly:
;
;   Is sizeof(thing-A of size 1) < sizeof(thing-B of size 1)?  No!
;   Is sizeof(thing-B of size 1) < sizeof(thing-A of size 1)?  No!
;
; Conclude that 1 == 1 and therefore these two are equivalent, and
; one of them can be discarded.  So this is an example of deduplication
; by size. When the compare is <= instead of <, then 1 <> 1 is inferred,
; and both items are kept, as they are different.

; Construct the Value
(define gt-dedupe
	(SortedValue
		(DefinedPredicate "greater-dedupe")
		(Name "item-list")))

; Print the list
(cog-value->list gt-dedupe)

; ----------------------------------------------------------

; The usual Atomese type constructors work with SortedStream.
; For the present case, it can be constructed using the LinkSignature.

(define link-sig
	(LinkSignature
		(Type 'SortedValue)
		(DefinedPredicate "not-less")
		(Name "item-list")))

; Construct it.
(define cnlt (cog-execute! link-sig))

; It works as before.
(cog-value->list cnlt)

; ----------------- That's all, Folks! The End! -----------------
