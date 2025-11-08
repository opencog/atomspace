;
; UNDER CONSTRUCTION INCOMPLETE
;
; flow-sorted.scm -- Presenting streams in sorted order.
;
; In most conventional usage, it is enough to process a stream in
; sequential order, on a first-in, first-out (FIFO) basis. Most
; Atomese stream processing will work in this way. There are two
; main exceptions: UnisetValue, and SortedValue. The UnisetValue
; is intended for item deduplication; it will sort items, but in
; an opaque fashion (nominally, in lexical order of what would have
; been the string, if the item had been printed.)
;
; The SortedValue allows a custom sort order to be applied, with the
; sorting operation specified in Atomese. The SortedValue is
; thread-safe buffer: it can be read from and written to simultaneously.
; The buffered contents will be kept (and returned) in sorted order.
;
; As a buffer, this is not really intended for sorting static lists,
; but for sorting items arriving on a stream.
;
; To provide meaningful stream buffering, it will pull from it's
; upstream source, attempting to "fill itself up". If the upstream
; source is empty but open, the reader will block. The buufer will
; remain in sorted order, and readers will be handed out in sorted
; order to readers.

(use-modules (opencog) (opencog exec))

; Create a stream that will sort items according to the
; size of their outgnoing set, the the largest items coming
; first. The GreaterThanLink provides the sort order; the
; SizeOfLink provides a numerical value that can be ordered.
; The comparison relation needs to be presented in such a way
; that the two inputs are clear; a LambdaLink is used for that.
(define order-relation
	(Lambda
		(VariableList (Variable "$left") (Variable "$right"))
		(GreaterThan
			(SizeOf (Variable "$left"))
			(SizeOf (Variable "$right")))))

; Create a list of items of varying sizes.
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


(define sorted-list
	(CollectionOf
		(LinkSignature (Type 'SortedValue) order-relation)
		item-list))


; Display it. Note that, at the bottom of the print, the current
; sample from the stream is printed. The stream advances every
; time it is referenced, and so repeated prints cause repeated
; references, advancing the stream.
(display fs)
(display fs)
(cog-value->list fs)
(cog-value->list fs)
(cog-value->list fs)
(display fs)
(display fs)

; By convention, streams are attached to fixed locations, where
; they can be found and accessed. For this demo, let's attach
; the stream to the Node "foo", located at the key "bar".
(cog-set-value! (Concept "foo") (Predicate "bar") fs)

; The attachment above works, but it is not "pure Atomese", it
; requires a call to `cog-set-value!` This can be avoided by
; using SetValueLink instead. However, since the stream itself is
; not an Atom, it cannot be place there directly. For this, re-wrap
; the list into a FlatStream, using the CollectionOfLink utility
; to perform the re-wrap.  When executed, the re-write will create
; the stream. The `cog-execute!` below results in exactly the
; structure as the `cog-set-value!` above; its just "pure Atomese".

(cog-execute!
	(SetValue (Concept "foo") (Predicate "bar")
		(CollectionOf (Type 'FlatStream) (OrderedLink item-list))))

; Streams can be referenced with ValueOfLink
(define ostream (ValueOf (Concept "foo") (Predicate "bar")))

; The Stream now advances by one with each reference to the stream.
(cog-execute! ostream)
(cog-execute! ostream)
(cog-execute! ostream)
(cog-execute! ostream)

; Reset the stream to the beginning. Earlier, the SetValue result
; was printed on the screen, referencing the stream, and advancing
; it to the second element. This accidental advance can be avoided
; by capturing the result and not printing it. (So, don't print "junk".)
(define junk
	(cog-execute!
		(SetValue (Concept "foo") (Predicate "bar")
			(CollectionOf (Type 'FlatStream) (OrderedLink item-list)))))

; Verify the stream really does start at the start.
(cog-execute! ostream)

; ----------------- That's all, Folks! The End! -----------------
