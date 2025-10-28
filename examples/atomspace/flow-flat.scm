;
; flow-flat.scm -- converting lists into streams.
;
; When presented with a list of Items, it is convenient to be able to
; serialize that list into a stream. This allows the down-stream
; processors to work on the list items, one by one. The FlatStream
; is a Value that, every time it is accessed, returns the next item
; out of a list.
;
; When the list is itself an item on a stream, it is flattened, till
; it is empty, and then the next list is fetched from the stream.
; That is, one intended use is to flatten streams of lists.
; The `parse-pipeline.scm` example in the `sensory` project
; demonstrates this.

(use-modules (opencog) (opencog exec))

; Create a list of items.
(define item-list
	(OrderedLink
		(Item "a")
		(Item "b")
		(Item "c")
		(Edge
			(Predicate "relation")
			(List (Item "d") (Item "e") (Item "f") (Item "g")))
		(Item "p")
		(Predicate "q")
		(TagNode "z")))

; Wrap the list with the serializer.
(define fs (FlatStream item-list))

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
