
Demo of Data Processing Streams
===============================
Examples of data processing networks.  These are networks, described
in Atomese, that apply transformations to streams of Values and Atoms.

These networks are used to process, transform, work with, remember,
discard, store, retrieve, send, memorize, summarize dynamic, time-varying
flowing data streams.  These are manipulated in the local, "current"
AtomSpace, but are generally data that arrives from sensory devices,
or are commands sent to "motors" to move, or agents to act.

Sensory streams can be rapidly time-varying (video, audio). There is
usually a combined motor element: one not only "looks"; one actively
steers the eyes towards "where to look".

* [`values.scm`](values.scm)                 -- Using Values and attaching them to Atoms.
* [`stream.scm`](stream.scm)                 -- Using a stream of time-varying Values.
* [`formulas.scm`](formulas.scm)             -- Representing arithmetic and computing Values.
* [`flows.scm`](flows.scm)                   -- Flowing Values around.
* [`flow-formulas.scm`](flow-formulas.scm)   -- Dynamically computing FloatValues.
* [`flow-futures.scm`](flow-futures.scm)     -- Dynamically updating Values.
* [`flow-string.scm`](flow-string.scm)       -- Flowing strings in and out.
* [`flow-flat.scm`](flow-flat.scm)           -- Deserializing lists into streams.
* [`flow-drain.scm`](flow-drain.scm)         -- Run a stream until it is empty.
* [`named-pipes.scm`](named-pipes.scm)       -- Using names to label data streams
* [`sorted-value.scm`](sorted-value.scm)     -- Sort items.
* [`count-pipeline.scm`](count-pipeline.scm) -- Long, multi-stage pipeline.
* [`table.scm`](table.scm)                   -- Fetching Values from a CSV/TSV table.
* [`multi-space.scm`](multi-space.scm)       -- Using multiple AtomSpaces at once.
* [`episodic-space.scm`](episodic-space.scm) -- Managing multiple AtomSpaces.
* [`bootstrap.scm`](bootstrap.scm)           -- Loading analytics pipelines.
