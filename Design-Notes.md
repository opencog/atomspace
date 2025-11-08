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

7 November 2025
---------------
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
Bingo -- works for FlatStream, too.

What's a stream? Well, its a Value, not an Atom. Lets look at examples:

  FormulaStream
     <executable atom returning numbers>

  FutureStream
     <executable atom returning anything>

Tap either of these once, they'll execute the atom that they wrap and
return the resulting value. The only difference is that FormulaStream
is explcitly types to be numeric.

  FlatStream
     <executable atom returning collection>

This one splits up collections into individual parts. Tap it once,
it executes the <executable atom> and returns the first item from
that collection.  Tap it again, you get the next item, until the
current collection goes dry.  Then it executes <executable atom>
to get the next collection.

The above three are simple types, and CollectionOfLink works great
for wrapping up that executable Atom with the StreamValue.

  SortedValue
     <lambda defining ordering relation>
     <dataset or source>

This is a challenge for CollectionOfLink. Before diving into that,
two more, that are planned but not yet implemented:

  GroupValue
     <lambda defining equivalence buckets>
     <dataset or source>

  AlwaysValue
     <lambda defining absolute equivalence>
     <dataset or source>

The GroupValue is meant to obsolete the GroupLink used by the query
engine. GroupLink was inspired by SQL GROUP BY and so was jammed into
the query engine, but doesn't belong there. The problem is that it
cannot fully sort results into buckets, until the query completes.
Thus, the user blocks, waiting on query completion. In the C++ code,
it appears as a post-processing wart. Not integrated with the query
in any way, just an extra step at the end. Should be sliced off, and
replaced as an optional post-processing step, implemented as a stream.

The AlwaysValue is meant to obsolete the AlwaysLink used by the query
engine. AlwaysLink ensures that *all* items in the result bucket share
a common property. If not, it returns null (no result). Its like a
GroupLink, followed by a test that there is only one group, and early
invalidattion if there is more than one. Its stapled on to the tail of
the sarch, not at all integrated in any way. Should be sliced off.

So we have three of these things. Now for the wicked part.

