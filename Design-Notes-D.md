Design Notes D - Analytics
==========================
Continuation of [Design Notes A](Design-Notes-A.md). January 2026

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

Here, by contrast, I wish to "observe" AtomApace contents. Since the
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

