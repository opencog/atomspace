Design Notes
============
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
is that more general, more powerful representations are found. To some
large degree, this has been a follow-your-nose process, with better
ideas surfacing relatively unobstructed and unencumbered. The decision
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
Today's issue: FilterLink vs. CollectionOfLink. They seem to conceptually
overlap, but its not clear if they can be unified or should be unified.

  FilterLink
     <matching clause>
     <dataset or stream>

When the <matching clause> is a RuleLink, it can do rewriting of the
data stream.  RuleLinks always have Variable declarations. When the
<matching clause> is a SignatureLink, it acts as a filter. Recall that
a SignatureLink is a type constructor for complex types.

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
SetLink. So, basically, unwrap and re-wrap with a different container
type. Note the container type is a simple type, not a compound type.

The first interesting change was the realization that CollectionOfLink
is just like PromiseLink, but more general. The PromiseLink was used to
create futures aka streams, but you can do this with CollectionOf, also.
This makes PromiseLink 100% redundant.  Example:

  CollectionOf
     (Type 'FutureStream)
     <dataset or source>

Executing the above returns a FutureStream that wraps the data source.
Each reference to the FutureStream returns an item from the source.
Bingo -- works for FlatStream, too.

Oh wait ... there is also [[LinkSignatureLink]] ... which also does
rewriting, but at the type level...

Streams
-------
This subsection has been copied into the
[wiki streams page](https://wiki.opencog.org/w/Streams). The wiki page
updates some of the discussion below.

What's a stream? Well, its a Value, not an Atom. Lets look at examples:

  FormulaStream
     <executable atom returning numbers>

  FutureStream
     <executable atom returning anything>

Tap either of these once, they'll execute the atom that they wrap and
return the resulting value. The only difference is that FormulaStream
is explicitly typed to be numeric.

  FlatStream
     <stream value of executable atom returning stream>

This one splits up collections into individual items. The stream is
assumed to be a stream of collections. A collection can be any Link
or LinkValue: something that holds a bunch of items.  Tap it once,
it gets a collection from the stream, and returns the first item from
that collection.  Tap it again, you get the next item, until the
current collection goes dry.  Then it taps it's source stream again,
to get the next collection.

The above three are simple types, and LinkSignatureLink works great
for wrapping up that executable Atom with the StreamValue.

  SortedStream
     <lambda defining ordering relation>
     <dataset or source>

There are two more that are planned but not yet implemented:

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
invalidation if there is more than one. Its stapled on to the tail of
the search, not at all integrated in any way. Should be sliced off.

The wiki page for GroupLink and AlwaysLink propose converting these to a
form of FilterLink. But this is not needed: the same ability is provided
by writing

    LinkSignature
       Type 'GroupValue
       <grouping relation>
       <data source>

So we have three of these things. Now for the wicked part.

 * FilterLink. The Filter's <matching clause> is also a lambda (in the
   broad sense; not just a LambdaLink, but any one of a variety of
   functional Atoms). The lambda is applied to items, one at a time.
   So, like srfi-1 filter-map.

 * The Sorted/Group/AlwaysValues are containers. The lambda is applied
   when items are inserted into the container. The container contents
   are time-varying.

 * FutureStream. Resembles a container, but is always empty. Calls
   `Atom::execute()` once, every time `FutureStream::value()` is called.
   That's the promise.

 * FlatStream. Resembles a container, but does NOT actually derive from
   ContainerValue. Pulls collections out of it's source stream.

 * QueueValue, UnisetValue. Thread-safe containers. Don't stream; they
   block until writer closes, then return one big gulp of everything.
   That is, they accumulate.

There is no FilterStream, because it is easy to set up FutureStream such
that it pulls from (pulls through) a FilterLink.

There is no need for a streaming QueueValue, UnisetValue. These can be
wrapped with a FlatStream, which will pull items out one at a time, or
will block, if the Container is empty and open. Thus, it provides the
items being added to the Container, without buffering.

The SortedStream works best when it drains its source, that is, keeps
it's source drained and empty, so that it can apply the sort order to
the buffered contents.

Then we have:

  DrainLink
     <executable atom returning anything>

It was invented to be the ultimate pull, the vacuum: it calls the
<executable atom> in an infinite spin loop, pausing only if the
execution blocks.

Questions:
 * Why is DrainLink an Atom, and not a Value, like SortedStream, which
   also drains? A: DrainLink is not a container; it can never fill up.
   A2: Historical accident.

 * Should DrainLink automatically launch its own thread? Probably yes.
   There should probably be a centralized thread pool, so that these
   can be managed. Right now, threading is ad hoc.

 * Is DrainLink mis-designed? Probably yes; it should loop on calls
   to `LinkValue::value()` and NOT `Atom::execute()` This is what
   FlatStream does.

A General question:

 * How should streams work?

There is an unresolved tension in the current implementation. There
seem to be two competing paradigms for streaming, and these are in
conflict.

 * Original vision for Values is that they could be time-varying;
   every access, by calling `Value::value()` might return something
   different.

 * Streams can run forever, or they can close. The `value()` method
   always returns a `std::vector<>` and if this vector is of size zero,
   it can be taken to be an end-of-stream indicator.  Note that
   RandomStream returns random numbers, and never closes. TextFile
   returns lines of text from a file, and closes on end-of-file.
   It can be put in a mode where it tails the file (forever).

 * DrainLink calls `execute()` on a source handle, to grab more data.
   This is a design mistake.

The issue is this: `execute()` returns a `ValuePtr`. If that is a true
stream, then calling `Value::value()` is a valid way to get the next
element. FutureStream converts calls to `value()` into calls to
`execute()`, and thus is the right tool for creating streams.

How should stream constructors work?
 * If the Stream constructor is given a Handle, then make make sure it
   is executable, call it and get a ValuePtr. That will be the source.
 * If the Stream constructor is given a ValuePtr, assume that is the
   source.

What is a source? Rather than generalizing, lets examine the
case-by-case needs. Lets start with FlatStream.
The `FlatStream::update()` method does this:

 * If the source is VoidValue or of size zero, return that.

 * If the source is a Link, then it is taken to be a finite source.
   The outgoing set is the "current collection", and items are doled
   out from it, one by one. When there are no more, VoidValue is returned
   to indicate end-of-stream.

 * If the source is a LinkValue, and is NOT a StreamValue, then it
   is taken to be a finite source, and treated like a Link, above.
   That is, call `LinkValue::value()` once to get the current collection.
   Iterate on that collection until empty, and then done.

 * If the source is a StreamValue and is not a ContainerValue, then
   it is assumed to be an endless (infinite) source. In this case, call
   `LinkValue::value()` to get the current collection. Dole it out,
   and, when empty, call `LinkValue::value()` again to get the next
   collection.

 * If the source is a ContainerValue, and the container is open,
   then call the container dequeue to get one item, and return that.
   This might block. If the Container is closed, just get everything.
   If the Container is closed and empty, return end-of-stream VoidValue.
   That is, a closed Container is taken to be a finite stream.

How about the SortedStream? Well, it seems like it will behave a lot
like a FlatStream, except it does sorting, and deduplication.

 * If the source is VoidValue or of size zero, return that.

 * If the source is a Link, then it is taken to be a finite source.
   Grab the entire outgoing set and stuff it into Uniset, so that it
   is sorted. Dole it out, one by one.

 * If the source is a LinkValue, and is NOT a StreamValue, then it
   is taken to be a finite source, and treated like a Link, above.

 * If the source is a StreamValue and is not a ContainerValue, then
   it is assumed to be a infinite source. In this case, a new thread
   is launched. It will make repeated calls to `LinkValue::value()`
   and the results will be added to the Uniset, until:
   -- (a) it blocks
   -- (b) it's size zero, denoting end-of-stream. Thread will exit.
   -- (c) a queue high-watermark is reached. Thread blocks until
          low-watermark is reached. The high-low watermarks are
          provided natively by cogutils `concurrent_set`.

 * If the source is a ContainerValue, then launch a thread for
   processing. If the container is open, then call the container
   dequeue in a loop, to get an item, and move it into the Uniset.
   This might block. If the Container is closed, just get everything,
   and close ourselves, too.

 * If we ourselves are closed and empty, return VoidValue.

OK, that sounds like a plan...

### GroupStream (or GroupValue?!)
I am implementing GroupStream now. It is based on SortedStream. To keep
the implementation slightly simpler, I will skip the puller thread for
now, because I think it won't be needed because the pattern matcher will
be managing it directly. This, mostly to skip a pointless copy step.

Oh, wait ... there's an unresolved tension between Stream and Value.
So ContainerValue (specifically, UnisetValue) will block until closed,
and this is how we wait for all MeetLink results to be returned. But
of course, we might want to stream these, too, so blocking until done
is wrong. Having Meet dump into a Stream is easy enough. Deduplication
will be lost if the consumer pulls too eagerly.

So now: should it be GroupValue, which is a ContainerValue that blocks
until closed? Or a GroupStream that blocks only when empty? Given one,
is there an easy way to convert into the other?

Answer: Lets do GroupValue, its easier, and the user could just use
FlatStream if they want to stream it out.

### Stream <--> ContainerValue conversion.
If we accept that ContainerValue semantics is "block until closed" and
stream semantics is "block when empty; indicate closing with VoidValue"
then what is the correct way to generically convert one into the other?

FlatStream will accept a ContainerValue and dole out elements from it
one at a time. So the conversion ContainerValue->Stream is solved by
FlatStream.

(This is a bit glib; will it actually work for SortedValue?)

Going the other way is unsolved, but easy: similar to DrainLink we
have DrainValue inherits from ContainerValue, polls forever, blocking
until it gets a VoidValue, and then it unblocks and returns the big
gulp.

But now there's additional confusion: Currently, ContainerValue inherits
from STREAM_VALUE but clearly these is wrong because we are now creating
different semantics for each. The appropriate solution seems to be to
declare two different SIGS: BLOCKING_SIG and STREAMING_SIG.

FLOAT_STREAM <- FLOAT_VALUE, STREAMING_SIG
STREAM_VALUE <- LINK_VALUE, STREAMING_SIG
CONTAINER_VALUE <- LINK_VALUE
QUEUE_VALUE <- CONTAINER_VALUE, BLOCKING_SIG
DRAIN_VALUE <- CONTAINER_VALUE

The Sigs partly solve some of the issues. All Values have always been
explicitly time-varying. The BLOCKING_SIG and STREAMING_SIG indicate
two different kinds of behaviors. The ContainerValue explicitly adds
thread-safe container manipulation functions. These can now be grouped
according to these two sigs.

Note that the distinction between STREAM_VALUE vs. LINK_VALUE has been
baked into sensory already, and elsewhere. The reasoning may have been
backwards: what was really needed was a LinkValue that truly was a
"constant" and thus could behave like a conventional OO struct.
(i.e. it just holds data; it does not change over time.)

Arguably, constant LinkValues "should have been" Atoms, but for various
vague reasons, we don't want these in any AtomSpace, and so they are
not; they're just "constant".

The above arguments suggest that SortedStream should be simplified into
a SortedValue, and that FlatStream should do the heavy lifting of
turning contents into a true stream.

### DrainValue
The above paints a simple picture of DrainValue: it is a container that
polls forever, and closes only when the input source closes. But I'm
confused:
* The above describes blocking semantics. But it could stream, just
  fine; there's no particular reason for it to block...
* The current SortedStream implements draining; Can this be removed and
  replaced by DrainValue? How?
* The natural mode for Uniset, GroupValue and SortedStream is to "hoard"
  -- to collect as much as possible to deduplicate, group or sort.
  So these should be characterized as being "drainers". The idea of
  "blocking" is a stand-in for their wanting to be hoarders.
* By contrast, QueueValue, nominally blocking, really doesn't care.
  It has no reason to hoard. The QueueValue could be (should be) a
  streamer...
* Perhaps the idea of BlockingSig is the incorrect abstraction?
  (Solution: rename to HoardingSig)

We seem to have multiple ideas, but its not clear how to combine them:
* MeetLink streams, until it is done, and then it signals being done by
  closing.
* The Uni/Group/Sorts are hoarders.
* Hoarders work "best" if the try to drain the upstream, and block/hold
  off the downstream.
* Hoarders can be streamers; as long as they are open, they can block if
  empty.
* Streaming and HoardingSig do seem incompatbile: both block, but in
  different places, for different reasnos.
* It only makes sense to be a HoardingSig if upstream is known to be
  finite; otherwise, low/high-watermark management is required (and is
  currently implemented in SortedStream.)

But what problem does the DrainValue actually solve? Certainly, the
SortedStream does not have to eagerly pull in a distinct thread; it can
pull lazily. That is, when polled, it can try-get from upstream. If it
finds something, it pulls that and places it into order.

TODO: Implement DrainValue ...

### Mixins
The above seem to be a collection of conflicting operational
requirements, which can be re-arranged in dozens of different
mix-n-match scenarios. The can be rolled up, hard-coded all-in-one
classes, like the current SortedStream, or modularized into Atomese
pieces that are assembled using ... Atomese.

Which raises issues of syntax. We need hoarders and streamers and
blockers and hi/lo container managers. Combining these as
multi-inherited c++ classes is not just unworkable in the long run,
it is already generating issues and problems in the present.
The bullet points above seem to describe a collection of mixins;
desirable aspects that are naturally combinable; but we have no Atomese
syntax for mixins.

Lets try some random ideas.
```
(DrainValue (SortedValue))
```
The drainer pulls from upstream, places results into SortedValue.
Downstream can work with SortedValue directly, or DrainValue can act as
a proxy for for whatever it is wrapping.

```
(FlatStream (SortedValue))
```
Provides the streaming semantics for SortedValue. Ummm...

I'm confused ... this starts resembling the chaining of stream
processing filters, except that it's done ... differently. Adding to
the confusion is that the current design for chaining filters is
awkward, confusing, verbose (as they go through anchor points).
So it seems what we really want is a better way of describing and
assembling streams.

### 25 December 2025 Status Update
Some notes about the current situation:
* Most of teh grunt-ugly stuff above has now been cleaned up.
* All of the big issues remain unsolved.
* `(FlatStream (SortedValue))` works great.
* `SortedValue` is very minimal.
* `RelationalValue` provides a fine base class for `SortedValue` and
  also `GroupValue`.
* `UnisetValue` drains. A distinct HoardValue could have been designed,
  but seems like overkill just right now.
* The draining is "on demand" rather than "continuous". This is worth
  commenting on. The "on demand drain" is exactly enough to fill up the
  Value, so that it can be drawn from. i.e. each draw first triggers a
  drain of upstream, refilling the hoarder, just before drawing on it.
  So a design maintaining temporal consistency, but lacking the power
  to be a driving engine, pulling/draining forever, independent of
  exernal forces.  i.e. unlike DrainLink, there's no thread that pulls.
* There's no `DrainValue`.
* `FlatStream` seems to hint at being a general streaming API. With
  flaws discussed earlier.

I think that's it. The code seems clean, well-organized within the
boundaries of the current unresolved meta-design flaws.


ObjectNodes
-----------
Historically, Atoms are necessarily stateless and immutable; this is
what allows them to have global uniqueness, thread-safety, etc. This
has been broken in two ways:

 * Every Atom has a Key-Value store, and this store is dynamic and
   mutable.

However, the Key-Value mechanism is a push mechanism: one pushes or
sets Values at a given key. This makes high-speed updates impractical,
since every update requires a mutex lock.

 * ObjectNodes are Atoms (Nodes) that support *-open-*, *-close-*,
   *-read-* and *-write-* messages, and sometimes more. These work
   with time-varying data. ObjectNodes are used primarily for connecting
   to external systems: StorageNode, for disk and network I/O, and
   the SensoryNodes, for external (environmental) sensorimotor systems.

Thus, ObjectNodes are valid Atomese Nodes, in that they are immutable
and globally unique. The static Key-Value store is supplanted by special
keys that fetch or set Values outside of local RAM.

The ContainerValue C++ class has open(), close(), add(), remove()
methods on it.

Questions:
 * Should ContainerValue be a kind of ObjectNode, instead?

This could solve a long-outstanding, painful issue in stream design.
The current stream architecture encourages small, modular stream
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
most things as streams, and version-half mostly tried to do everything
with nodes and messages. The problem with messages is that they become
"line-oriented" instead of "streaming".  In the end, the sensory API
still creates (anonymous) streams that have to be anchored.

The issue here is that calling `StreamValue::value()` is fast: call it,
and you get the stream value you wanted; done.  If this was replaced
by `StreamLink::getValue(Predicate "*-read-*)`, this requires
dispatching on the Predicate to find .. what? Return what? It's
indirection ...


Recap
-----
So lets recap the issues:

 * Authoring pipelines is blisteringly hard.

 * Having non-anonymous ObjectNodes that do what Values do might
   simplify the authoring of pipelines.

 * There's some unresolved tension with the overlapping duties of
   CollectionOf and FilterLink. Specifically, of the CollectionOf
   type specification got fancy, got lambda-ish, it would start
   resembling a filter.


Assembly Theory
===============
Start all over again.

So an old idea is that the assembly of parts should work like jigsaw
pieces.  Each jigsaw connector should have some type description. This
is some mashup of type theory, ideas from Link Grammar, and ideas like
introspection from Java or from D-Bus.  That, at least, is the sexy
idea. The actual practice is anything but -- just a lot of carefully
designed, hand-built pipelines. I got enough of the idea across that
Calude could build some of them, but I had to supervise.  There's no
general assembly mechanism, and this remains far off and unworkable
despite repeated attempts.  What's going wrong?

Two or three things seem to be desirable:
* Something other than the use of anchors as attachement points where
  SetValue and ValueOf can rendevous.
* Pipelines need to be descriptively ddefined.
* It would be OK if there was some constructor that performed the actual
  wiring, given the description. That is, the description itself does
  not have to be runnable; it just needs to be compilable/assemblable
  into something that runs ...

There are two meta-goals I have to decouple:
* The short-medium term issue of making pipelines defacto easier to
  write, over the coming months. Ideally using some descriptive
  framework having some OK properties.
* The long-term goal os self-assembly and recursive algorithmic
  self-design.

Pipeline Assembly
-----------------
So what happened when I wrote `(FlatStream (SortedValue))`?
* I designed a c++ class called `FlatStream`, whose ctor takes a
  single Value. It pulls from that Value, by driving it's `update()`
  method.
* There is (currently) no Atomese description of this API, although
  both `FlatStream` and `SortedValue` have SIG's and ARG's that allow
  them to be attached via the class factory.
* These SIGs, ARGs and the general type hierarchy is NOT stored in the
  AtomSpace.
* Despite things like TypeInhNode, etc. there is no actual way that
  an Atomese expression can be written to walk/explore/analyze the
  type hierarchy.
* There is no definition, either hand-written or auto-generated, that
  expresses (re-expresses) the SIG/ARG type constrctors/constraints
  in terms of `Connector`, `ConnectorSeq`, `Section` that we've been
  giving lip-service to.
* There is no way to take a description written in terms of Connectors
  and converting it into an actual assembly like
  `(FlatStream (SortedValue))`

FWIW, There's the unexplored alternative of hooking these up with
the existing anchor-point design.
```
(cog-execute! (SetValue (Anchor "foo") (Predicate "key")
	(LinkSignatue (Type 'SortedValue) ...?))

(cog-execute!
	(LinkSignature (Type 'FlatStream) ...
		(ValueOf (Anchor "foo") (Predicate "key"))))
```

* There's no way to convert the anchor point design into a
  hierarachical design, or back.
* Any given anchor-point connection is not validated via the SIG
  mechanism. Invalid anchor-point connections are easily constructed.

Wow. Those are like six really damning design flaws. That I have not
had to face yet. How did that happen?

What's the priority order for fixing any of this?

Connectors and Pipes
--------------------
To arrive at a connector-based syntax for hooking up pipelines, lets
start with a simpler, more "obvious" solution. Consider having an
`InputNode`. It can be used anywhere a stream value is used, and acts
as a place-holder or "promise" for future input. Thus, one can write
```
   (FlatStream (InputNode "input-for-flattener"))
```
Polling `FlatStream` would call `InputNode::update()` which then blocks,
as no actual source of input is connected.

The directionality of `Input` and `Output` labels is mildly confusing,
while `ConsumerNode` and `ProducerNode` are perhaps less so?

To provide an input, one writes:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ProducerNode "output-from-sorter"))
```
Creating this PipeLink and placing it in the AtomSpace would wake up
the `ConsumerNode` (via c++ `std::condwait` on some `std::mutex`) and
then (magic happens here) it gets stream data from `OutputNode`.

How might that work? Well, lets try this:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ValueOfLink (Anchor "some place") (Predicate "some key")))
```
which should "work as expected", except that we're once again using
Anchors, which is a design point I kind of want to move away from.

Side comment: The `ValueOf` returns `VoidValue`, unless a `SetValue`
was done earlier. The `VoidValue` is already used as an end-of-stream
marker, so the consumer would immediately receive an end-of-stream
unless the producer had been previously declared. There's no obvious
way  of fixing this without inventing some `WaitVauleOf` that camps
on that key, waiting for something to be attached. This does need to
be fixed; we do want to be able to define flows in arbitrary order,
so that they can e.g. be retreived from Storage.

Lets try some others:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(MeetLink ... pattern...)
```
Clearly this wires up the output of the `Meet` to the consumer. No

Another:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ExecutionOutput ... )
```
This wires up the output of the `ExecutionOutput`. No problems here.

There is no obvious design for a `OutputNode` aka `ProducerNode`.
So the initial motivating example above is flawed.

Earlier designs used `Lambda`s:
```
    (Define (DefinedProceedureNode "named function")
       (Lambda
           (VariableList ... inputs ...)
           (... body ...)))
```
Here, the input variables are explicit placeholders; but they only
provide half of what `ConsumerNode` provides: we know what the inputs
are, but we don't know where they "came from".

The `ExecutionOutput` provide "half" of `PipeLink`:
```
   (ExecutionOutput
      (DefinedProcedure "named function ")
      (... producers ...))
```
The output of the execution is not routed "anywhere", it just arrives
"here", an unspecified, anonymous "here and now" when `FooLink::execute()`
is called.

   (CollectionOf
      (Type 'FormulaStream)
      (ExecutionOutput (DefinedProcedure "named function ")



SortedValue
