Design Notes
------------
The design and implementation of the AtomSpace has been ongoing since
about 2002, when it was first created as a workspace for controlling
small animated virtual creatures. In the decades since, it has seen
many changes; the high points the ideas that worked out and are
generally useful to a broad audience are listed in the main README.md
file. Many ideas did not work out: the OpenCog wiki lists 170 different
Atom types that are implemented in this and related code bases. It lists
120 more marked as obsolete: knowledge representation and data
processing ideas that did not work out.

One of the strongest reasons that initial concepts fall by the wayside
is that more genreal, more powerful representations are found. To some
large degree, this has been a follow-your-nose process, with better
ideas surfacing relatively unobstructed and unencoumbered. The decision
process for these are lost in the sands of time: mailing list posts,
github issues, wiki pages, private conversations.

No archeology will be done. This file is for new and annoyingly
difficult design issues that present themselves.  It is being written
because writing often helps clarify the issue. It is being written
because just lounging back and thinking hard seems to not be enough.

The primary trouble-maker is the concept of streams, flows and futures.
Plenty of simple stream processing demos have been created, but new
problems keep popping up.

November 2025
-------------
Today's issue: FilterLink vs. CollectionLink. They seem to conceptually
overlap, but its not clear if they can be unified or should be unified.

  FilterLink
     <matching clause>
     <dataset or stream>

When the <matching clause> is a RuleLink, it can do rewriting of the
data stream.  RuleLinks always have Variable declarations. When the
<mathcing clause> is a SignatureLink, it acts as a filter. Recall that
a SignatueLik is a type constructor for complex types.

  CollectionOfLink
     <type spec>
     <dataset or stream>

The CollectionOfLink does type rewriting, changing the type of the
objects in the data stream, without the pesky RuleLink.

The original, prototypical use was

  CollectionOfLink
     (Type 'Set)
     (LinkValue ...)

which just took the contents of LinkValue, and stuffed them into a
SetLink. So, basically, unwrap and rewrap with a different container
type. Note the container type is a simple type, not a compund type.

The first intersting change was the realization that CollectionOfLink
is just like PromiseLink, but more general. The PromiseLink was used to
create futures aka streams, but you can do this with CollectionOf, also.
This makes PromiseLink 100% redundant.  Example:

  CollectionOf
     (Type 'FutureStream)
     <dataset or source>

Executing the above returns a FutureStream that wraps the data source.
Each reference to the FutureStream returns an item from the source.
Bingo -- the high-falutin Promise was doing the same thing.

