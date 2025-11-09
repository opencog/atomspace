;
; flow-sorted.scm -- Presenting streams in sorted order.
;
; In most conventional usage, it is enough to process a stream in
; sequential order, on a first-in, first-out (FIFO) basis. Most
; Atomese stream processing will work in this way. There are two
; main exceptions: UnisetValue, and SortedStream. The UnisetValue
; is intended for item deduplication; it will sort items, but in
; an opaque fashion (nominally, in lexical order of what would have
; been the string, if the item had been printed.)
;
; The SortedStream allows a custom sort order to be applied, with the
; sorting operation specified in Atomese. The SortedStream is
; thread-safe buffer: it can be read from and written to simultaneously.
; The buffered contents will be kept (and returned) in sorted order.
;
; As a buffer, this is not really intended for sorting static lists,
; but for sorting items arriving on a stream. However, for the present
; example, sorting will be applied to statiic lists.
;
; It seems that the only meaningful way to apply sorting to a stream
; is to pull as much as possible from the stream, and sort that. The
; SortedStream does exactly that: it pulls as much as possible from
; the upstream source, and puts everything it gets into sorted order.
; To avoid blocking when the upstream source blocks, the pull is done
; from a separate, privately-maintained thread. To avoid overflowing
; when the upstream producer is fast, and the downstream producer is
; slow, high-low watermarks are used on the sorted buffer. These are
; currently hard-coded.

(use-modules (opencog) (opencog exec))

: Demoing a running stream is difficult, and so the demo below will
; demonstrate sorting on a static list. This has little overall impact.
;
; The "stream" to be sorted will consist of a collection of Atoms of
; varying sizes. The sort function will examine the sizes, and order
; accordingly. Several variants of the sort function are demoed:
; ascending, descending and "deduplicating". The deduplicating order
; is curious: it only admits one exemplar of a given size in the stream.
; Unlucky Atoms that happen to be of the same size, but are otherwise
; different, are discarded. This deduplication is the same as that
; provided by the UnisetValue, although that one deduplicates based on
; the global uniqueness of Atoms.
;
; The ordering relations will be created with the LambdaLink. This
; defines two variables: the left and right variable; which can then be
; used in arbitrarily complicated expressions in the body. For the demo,
; the SizeOfLink provides a numerical value for the size of an Atom;
; the GreaterThanLink, LessThanLink, EqualLink and the boolean ops
; AndLin, OrLink can be combined.
;
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

; Create a list of items of varying sizes.
(define item-list
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

; Construct the stream.
(define ge-stream (SortedStream greater-or-equal-relation item-list))

; Access the stream. Each access returns a list holding one item.
; The largest is returned first.
(cog-value->list ge-stream)

; Subsequent accesses returns the next and the next ... and eventually
; the empty list.
(cog-value->list ge-stream)
(cog-value->list ge-stream)
(cog-value->list ge-stream)
(cog-value->list ge-stream)
(cog-value->list ge-stream)
(cog-value->list ge-stream)

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
(define not-less-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Not
			(LessThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

; Should work as before.
(define nlt-stream (SortedStream not-less-relation item-list))

; Access the stream.
(cog-value->list nlt-stream)
(cog-value->list nlt-stream)
(cog-value->list nlt-stream)
(cog-value->list nlt-stream)

; --------------------------------------------------------
; Make sure this is not a happy accident, and reverse the order.
(define not-greater-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(Not
			(GreaterThan
				(SizeOf (Variable "$left"))
				(SizeOf (Variable "$right"))))))

; This time, the smallest come first.
(define ngt-stream (SortedStream not-greater-relation item-list))

; Access the stream.
(cog-value->list ngt-stream)
(cog-value->list ngt-stream)
(cog-value->list ngt-stream)
(cog-value->list ngt-stream)

; --------------------------------------------------------
; Now for fun and games: deduplication.
(define greater-relation
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
; by size. When the compare is <= instead of <, then 1 <> 1 is infered,
; and both items are kept, as they are different.

; Construct the stream
(define gt-stream (SortedStream greater-relation item-list))

; Access the stream.
(cog-value->list gt-stream)
(cog-value->list gt-stream)
(cog-value->list gt-stream)
(cog-value->list gt-stream)

; ----------------------------------------------------------

; The usual Atomese type constructors work with SortedStream.
; For the present case, it can be constructed using the LinkSignature.

(define link-sig
	(LinkSignature (Type 'SortedStream) not-less-relation item-list))

; Construct it.
(define cnlt (cog-execute! link-sig))

; It works as before.
(cog-value->list cnlt)
(cog-value->list cnlt)
(cog-value->list cnlt)
(cog-value->list cnlt)

; ----------------- That's all, Folks! The End! -----------------
