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

 * FilterLink. The Filter's <matching clause> is also a lambda (in the
   broad sense; not just a LambdaLink, but any one of a variety of
   funtional Atoms). The lambda is applied to items, one at a time.
   So, like srfi-1 filter-map.

 * The Sorted/Group/AlwaysValues are containers. The lambda is applied
   when items are inserted into the container. The container contents
   are time-varying.

 * FutureStream. Resembles a container, but is always empty, and pulls
   from upstream, whenever it is pulled from. That's the promise.

 * FlatStream. Resembles a container, but does NOT actually derive from
   ContainerValue. Pulls only when it needs to.

There is no FilterStream, because it is easy to set up FutureStream such
that it pulls from (pulls through) a FilterLink.

The SortedValue works best when it drains its source, that is, keeps
it's source drained and empty, so that it can apply the sort order to
the buffered contents.

Then we have:

  DrainLink
     <executable atom returning anything>

It was invented to be the ultimate pull, the vacuum: it calls the
<executable atom> in an infinite spin loop, pausing only if the
execution blocks.

Questions:
 * Why is DrainLink an Atom, and not a Value, like SortedValue, which
   also drains? A: DrainLink is not a container; it can never fill up.
   A2: Historical accident.

 * Should DrainLink and SortedValue automatically launch their own
   threads? Probably yes. There should probably be a centralized
   thread pool, so that these can be managed. Right now, its ad hoc.

Historically, Atoms are necessarily stateless and immutable; this is
what allows them to have global uniqueness, thead-safety, etc. This
has been broken in two ways:

 * Every Atom has a Key-Value store, and this store is dynamic and
   mutable.

However, the Key-Value mechanism is a push mechanism: one pushes or
sets Values at a given key. This makes high-speed updates impractical,
since every update requires a mutex lock.

 * ObjectNodes are Atoms (Nodes) that support *-open-*, *-close-*,
   *-read-* and *-write-* messages, and sometimes more. These work
   with time-varying data. ObjectNodes are used primarily for connecting
   to external systems: StorageNode, for disk and newtwork I/O, and
   the SensoryNodes, for external (environmental) sensorimotor systems.

Thus, ObjectNodes are valid Atomese Nodes, in that they are immutable
and globally unique. The static Key-Value store is supplanted by special
keys that fetch or set Values outside of local RAM.

The ContainerValue C++ class has open(), close(), add(), remove()
methods on it.

Questions:
 * Should ContainerValue be a kind of ObjectNode, instead?

This could solve a long-outstanding, painful issue in stream design.
The current stream architecture enourages small, modular stream
transformations, with source data obtained by executing ValueOf.
But since each of these streams are Values, they are lost if they
are not placed somewhere: thus, by convention on some anchor point
(arbitrary atom, e.g. Node having arbitrary name) and then a specific
Key on that anchor: a second arbitrary Atom, typically a Node with
arbitrary name. So, two arbitraries.

If a ContainerValue was an ObjectNode, then:
 * It would be stored in the AtomSpace
 * It could always be found with it's unique name.

Eliminating ContainerVaules is awkward, because the query engine API
is simpler with anonymous containers; i.e. it would be awkward to force
the query engine to select some random name or uuid. Yuck. Anonymous
objects are useful.

Question:
 * Should *every* Value have a corresponding non-anonymous ObjectNode?

Seems like "yes" would be a good answer. Except that this would create
an explosion of new types, the Object types. Whoa.

Stream access currently works like this:

  (ValueOf (Anchor "some name") (Predicate "some key"))

which returns some Value, e.g. FlatStream, or whatever, which is then
sampled by reference.  This would be replaced by:

  (ValueOf (ContainerObject "some obj name") (Predicate "*-read-*"))

which returns... uhhh? ... one item from the stream? Already went
through this round of confusion with sensory, where v0 implemented
most things as treams, and version-half mostly tried to do everything
with nodes and messages. The problem with messages is that they become
"line-oriented" instea of "streaming".  In the end, the sensory API
still creates (anonymouos) streams that have to be anchored.

The issue here is that calling `StreamValue::value()` is fast: call it,
and you get the stream value you wanted; done.  If this was replaced
by `StreamLink::getValue(Predicate "*-read-*)`, this requires
dispatching on the Predicate to fid .. what? Return what? It's
indirection ...


Recap
-----
So lets recap the issues:

 * Authoring pipelines is blisteringly hard.

 * Having non-anonymous ObjectNodes that do what Values do might
   simplify the authoring of pipelines.

 * We still don't have a good answer for SortedValue vis-a-vis
   CollectionOf.

 * There's some unresolved tension with the overlapping duties of
   CollectionOf and FilterLink. Spcifically, of the CollectionOf
   type specification got fancy, got lamba-ish, it would start
   resembling a filter.

