Design Notes D - Analytics (Introspection)
==========================================
Continuation of [Design Notes A](Design-Notes-A.md). January 2026

Analytics is when one has a pile of data, and want to know "what is in
there". Introspection is when that pile of data is "yourself". I need to
perform analytics on large jumbles of Atomese data, and this analytics
needs to be written in Atomese, thus opening the door for recursive
introspection. (e.g. solving quotidian issues like "where the heck did
I put my analytics tools?" by using uhh, analytics tools to find them.)

Analyzing AtomSpace Contents
----------------------------
I am now facing an analytics crisis. I have an AtomSpace, filled with
data. I have some Atomese data processing pipelines, such as the one in
`examples/flow/count-pipeline.scm` that defines a search pattern, uses
it to feed a processing pipeline, and then, eventually, displays the
data in graphical form. For the sake of discussion, lets assume the
visualization is to happen in a web gui, such as the visualizer in
[the cogserver visualizer](https://github.com/opencog/atomspace-viz).

The Atomese processing pipelines should be thought of as consisting of
processing parts (written in Atomese) connected with wires (written in
Atomese). The parts can be thought of as an electronics BOM, while the
wires can be thought of as an electronics netlist.

The crisis has many components:
1. Where do these circuits (and there will be many) live and be stored?
   For now, I guess as flat files in the `atomspace-viz/analytics`
   directory, but long term, in some AtomSpace.
2. How are they moved/copied from thier starting location, to the
   AtomSpace on which they are applied? How are they "loaded"?
3. How do they perform thier observation and processing without
   polluting the base AtomSpace to which thier processing is being
   applied?
4. How are the results to be reported back to the visualizer?

This has some of the flavor of the earlier sensorimotor design
ruminations, but is quite different in some fundamental ways. So, the
current set of sensorimotor examples include a file system
viewer/crawler, an IRC chat API, and a text terminal API. These are all
obviously "external" unix subsystems, with conventional API's that only
need to be mapped to the Atomese `ObjectNode` API.

Here, by contrast, I wish to "observe" AtomSpace contents. Since the
contents can be "anythying", a big part of observation will be figuring
out what it is that is being looked at.  This has some of the flavor of
both the old "learn" project, and of the old pattern-mining project. The
starting point is to simply "observe" how many Atoms there are, of
different types. That's what the `examples/flow/count-pipeline.scm`
script does, but deploying it promptly founders on the four issues
above. After this, I guess there is the need run come sort of queries to
determine, I dunno -- how ListLinks, PredicateNodes, etc are deployed.
For now, I plan to wing it -- just try stuff and see how it works out.
So the primary crisis issues are the ones listed above.

Why is this a crisis? Well, because every obvious easy brute-force
solution is obviously inadequate. That is, it could be brute-forced and
hacked, but that defeats the entire raison-d'etre. Asking Claude to write
yet more unmaintainable spaghetti code is antithetical to the project
intent. Just to be clear: a huge aspect of Atomese is that it enables
introspection. I'm now saying put-up-or-shut-up -- how does that
introspection work?  In the past, I was observing word counts. In the
past, I sketched several reasonable designs for observing audio and
pictures. But now I want to observe Atomese. How?

### Persistance
Where does this analytics code "live"? The simple answer is "wherever
`make install` installs it. This is the super-conventional answer, and
reinforces the traditional file-system vs. OS kernel split. The OS
kernel manages the "live", executing threads; the file system stores
both "dead" data and non-executing code. The barrier between the two is
managed by linker-loader runtimes: glibc/binutils, java, python, guile,
etc. All systems that I know of use files; none store binaries in
databases (except possibly Microsoft...)

Files are "human friendly", and are easy to understand and navigate.
Just keep in mind `cd` and `ls` and you're done. Maybe `pwd` rarely.
Unsophisticated users has the MacOS Finder; Linux has the Midnight
Commander.  Android and iPhone users are unaware that file systems
exist; they navigate screens of apps which hide & abstract away the
storage structure.

So, to resolve version 0.1 of this crisis, I can `make install` some
flat `*.scm` files into the install dir. When the CogServer powers up,
and the user aims the browser at it, those files are loaded as-needed
by cogserver infrastructure (below). This is adequate for
boot-strapping.

Long-term, the analytics code should be stored in a `RocksStorageNode`
(which is built on RocksDB, which stores contents in ... a directory
filed with files. But the RocksDB provides its own loader and file
management system, so I do not have to deal with any of that.) So why
store in `RocksSorageNode`? Because that allows a more sophisticated
data management infrastructure to kick in. I can go meta-meta on the
data analytics. I can ask "what is inside of there?" and something can
look, and figure it out.

This is a non-trivial point, so I will canoodle on it for a while.
Conventional flat files are "opaque", in that `cd` and `ls` only tell
you about location, time-stamp and size, but not contents. To know the
contents, you have to open and read it, using whatever format reader is
appropriate. These readers are all highly specialized: reading java
bytecode is very different than reading an MP3 file. Both file formats
come with directory info: Java uses jar which is a tar file plus some
extras stuff, while glibc uses `ar`. MP3 files include metadata, I'm not
sure how.

If I just dump a bunch of `*.scm` files into a directory, there is almost
no knowledge retained of what they contain. I could ask Claude to open
them, and read them, and tell me. I am *this close* (holds fingers
together) to letting Claude be my file-system manager, except that
Claude can't remember for shit, so I need to create a memory subsystem
that is something more than a tumble-down jumble of text-file prompts.

And this is why I should not store Atomese as scm files. Store them in
`RocksStorageNode` and then build up the needed infrastructure to
introspect the contents.

### Make install
The above suggests that the perhaps a short-term bootstrap solution is
to `make install` the Atomese scm files to a well-known directory, and
defer using `RocksStorageNode` until later. This then raises the issue
of "how do I load those scm files in the CogServer context?", as they
are needed to run the analytics pipeline.

Thus, rather than solving the "how do I load flat files into a running
cogserver?" question, it now seems infinitely preferable to instead ask
"how do I attach a RocksStorageNode to a running cogserver?" This avoids
the invention/creation of weirdo ad-hoc code for loading text files,
while also staying within the project paradigm of designing systems in
pure Atomese.

That is, the CMakefiles need to create the `RocksStorageNode` locally,
during build, and then `make install` needs to copy it into place in the
file system. Perhaps to `/usr/local/share/cogserver/analytics`. This
feels doable. Some caution with versioning: make install needs to wipe
out the old directory contents, before copying in the new contents, as
otherwise there is corruption. This would be for Debian/Redhat; the guix
system has it's own versioning system that avoids this issue.

### Runtime loading
Now comes the tricky part. The cogserver is started, the user loads a
bunch of data into the cogserver, and then the analytics package is
started. How does this work? The requirements seem to be these:

* Create an `ObserveNode` (inheriting from `ObjectNode`, maybe
  `SensoryNode`, err, well, no see below.) specialized for opening the
  `analytics.rdb` file and loading it.
* This node needs to do as much work as possible in a child AtomSpace
  of whatever the cogserver is holding.
* The web interface/network server needs to operate with this child
  AtomSpace, and not the main one. Do we need to have a custom
  `NetworkServerNode` for this? or perhaps a brand-new `CogServerNode`?
  (port numbers get bumped...)

So ...the `SensoryNode` ... how does it interop with the CogServer to
get started?

Should it be a `SensoryNode`? A `StorageNode`? Something else,
subclassing from `ObjectNode`? The `StorageNode` backend does have a
remote-execution callback. Lets figure out what it needs to do, and
what the methods on it need to be.

What does `SensoryNode` provide? Some c++ virtual methods: open, close,
read, write, barrier. ... These are unix-inspired but are not obviously
appropriate for the present case. Maybe open, close, but the read/write
idea do not seem transferable to this scenario.

The `StorageNode` has some huge number of methods; most seem
inappropriate... Well, running wild with imagination suggests some of
these could be used to copy between disjoint AtomSpaces, but this is
premature at this point in design space. They do, however, tackle the
interesting task of "what do I copy, when I don't know what is held at
the remote end?" So, for example, we "don't know" what is in Rocks,
until it is actually loaded. We don't know what is in a remote server,
till it is transfered. The backing store has a "execute remote query"
which is meant to partially solve this problem, and so perhaps is what
is needed here, as well. It defines `BackingImplicator` derived from
`Implicator` and is implemennted in `persist/api/BackingQuery.cc`
It performs graph traversal with explicit calls to `getIncomingSet()`
whenever needed.

More design details:

* `ObserveNode` or maybe `ExamineNode` inherits from `ObjectNode`.
* It has to be given two things: (a) the `StorageNode` containing
  the probe code, (b) the `CogServerNode` that will allow direct
  communications.
* We could stick (b) in (a), except that port numbers need to be
  configurable.
* Perhaps it should be an `ObserveLink` so that (a) and (b) can be
  provided. Is there any reason for it to be an `ObjectNode`, anyway?

If it is a `Link`, then `::execute()` needs to:
* Create a child AtomSpace
* Open the `StorageNode`, and load everything into the child.
* Start the pre-configured `CogServerNode`
* But this is done only once; i.e. there should be both `*-open-*` and
  `*-close-*` since open and close need to be idempotent.
* So it needs to be `ObserveNode` (or `AnalyitcsNode`?)

Does this need to be a new Node type, or can we do it entirely with
pure Atomese?  Lets try it. This is a "bootstrap" sequence, attempting
to launch and then populate a child AtomSpace, with minimal invasion of
the parent AtomSpace.
```
(use-modules (opencog) (opencog persist) (opencog persist-rocks))

; Create a handle to a child AtomSpace we can hold onto.
; This always returns one and the same child space (which is good)
; but it prevents two different users from having their own copy
; (which is bad).
(Pipe
	(Name "make-as")
	(AtomSpace (AtomSpaceOf (Link))))

; The StorageNode is where the data will reside. We open it in such a
; way that when data is loaded from it, it will go into the child space.
(cog-execute! (SetValue
	(RocksStorageNode "rocks:///tmp/foo")
	(Predicate "*-open-*")
	(Name "make-as")))

; Load data into the child space. All sorts of scripts can be loaded
; up, including the CogServerNode. Two problems, though: (a) the port
; numbers on the CogServerNode would need to be configured, and (b) the
; "*-start-*" message needs to be sent to it.
(cog-execute! (SetValue
	(RocksStorageNode "rocks:///tmp/foo")
	(Predicate "*-load-atomspace-*")
	(Link)))
```
