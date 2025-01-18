Design Notes
============
Notes about OpenCL interfaces and how they impact Atomese design.

* Vectors will need a corresponding (private) `cl::Buffer` object
  (per vector).  The `cl::Buffer()` ctor requires a `cl::Context`.
  Some `cl::Buffer` ctors don't require a context; these all use
  the default context instead (seems like a bad idea, for us...)

* Alternative is to use SVM "Shared Virtual Memory", The ctors
  also need `cl::Context` and if not supplied, uses the default.

* The kernel itself needs only a `cl::Program` which holds the
  actual kernel code.

* Kernels are executed by placing them onto a `cl::CommandQueue`.
  This queue takes both a `cl::Context` and also a `cl::Device`
  in it ctor. Kernels are executed async.

* Obtaining results from the exec needs a `cl::Event`, created when
  the kernel is enqueued.

Design alternatives
-------------------
Different ideas for communicating with GPUs.

* Use `GroundedProceedureNode`s to wrap kernels. Old-style, yucky.
  Why yucky? Because its stateless: besides the string name of the
  node, nothing is stored in the atom. There's no open/close phasing.
  Its just a function call. Maps poorly onto stateful I/O.

* Use `StorageNode`s to wrap kernels. This presents other issues.
  First, traditional `StorageNodes` were meant to send/receive atoms,
  while here, we need to send/receive vectors represented as
  `FloatValue`s.  Maybe other kinds of vectors, but for now,
  `FloatValue`s.  The other problem is that the `StorageNode`
  API sits outside of Atomese proper, and uses instead a collection
  of calls `cog-open` and `cog-close` to manage connection state.
  Which is adequate for the intended use, but leaves something to be
  desired.

* Create something new, inspired by `BackingStore`. This removes some
  of the issues with StorageNodes, but leaves others in place. One
  is that, again, the API sits outside of Atmoese. The other is that
  this becomes yet another engineered solution. Of course, engineered
  solutions are inescapable, but... for example: the traditional unix
  open/close/read/write 4-tuple provides an abstraction that works very
  well for many I/O tasks: open/close to manage the channel, and
  read/write to use it. But the OpenCL interfaces do not map cleanly
  to open/close/read/write. They use a more complex structure.

The abstraction I'm getting is this: open near point, open far point,
select far-point message recipient, send message. That mapping would
have psuedocode like so:
```
   open_near_point() {
      Using cl::Device
      Create cl::Context and retain pointer to it
   }

   open_far_point() {
      Using cl::Context from near point, and externally specified
      cl::Device, create a cl::CommandQueue. Retain pointer to the
      cl::CommandQueue. Its the commo handle.
   }

   select_recipient() {
      Create a cl::Event as the commo handle, retain pointe to it.
      Use the externally supplied cl::Program, retain pointer to it.
   }

   write() {
      Using the selected recipient, create a cl:Kernel.
      Call cl::Kernel::setArgs() in th kernel to establish the connection.
      Call cl::CommandQueue::enqueueNDRangeKernel() to perform send
   )

   read() {
      Using the selected recipient, wait on cl::Event
   }
```
Caveat: the above is already an oversimplification of the OpenCL
interfaces, because a `cl::Conext` is not created out of thin air, but
requires a vector of `cl::Device` in it's ctor. And devices need
`cl::Platform`. Lets ignore these additional complexities, for the
moment.

The above API is more complex than open/close/read/write. There are
three choices:
* Codify the above as "sufficiently generic", claiming that e.g. CUDA
  would also fit into this model.
* Recognize the above as a cascade of opens(), each requiring the prior
  so that it resembles the peeling back of an onion.
* Recognize that the peeling-of-an-onion model is too 'linear', and that
  there is a network of interactions between the various `cl::` classes.
  That network is not a line, but a DAG. Encode the DAG as Atomese.
  That is, create an Atom that is more-or-less in 1-1 correspondence
  with the OpenCL classes. Communication then requires connecting the
  Atomese DAG in the same way that the OpenCL API expects the
  connections.

I like this third option. But how would it work, in practice?

Wiring Diagrams
---------------
Four choices for wiring diagrams:
* `EvaluationLink` -- function-call-like
* `RuleLink` -- inference-like
* `FilterLink` -- electronics-circuit-like
* `Section` -- grammatical/sheaf-like

Pros and cons:

### EvaluationLink
Olde-school. Uses  `PredicateNode` for the function name, and a list
of inputs, but no defined outputs. Can be given a TV to indicate
probability, but no clear-cut interpretation of the function arguments.
Replaced by `EdgeLink` for performance/size.

### RuleLink
Originally intended to be a base class for rewriting (for PLN).

* Fairly compact.
* Distinct "input" and "output" areas.
* Rules are nameless (anonymous) with no built-in naming mechanism.
* Rules are explicitly heterosexual (everything is either an input, or
  an output, and must be one or the other.) This is problematic if
  inputs or outputs are to be multiplexed, or given non-directional
  (monosexual) conntions.
* No explicit connection/glueing semantics.

`RuleLink`s specify term rewrites. Ideal for forward-chained rewriting.
With great difficulty, can be backwards-chained. That this was a diffculty
was exposed by PLN development.

### FilterLink
The `FilterLink` is used to specify a filter that can be applied to a
stream.  The stream can be Atoms or Values or both. (This distinguishes
it from the `QueryLink`, for which the source and target must lay within
the AtomSpace itself.)

Wrapping a `RuleLink` with a `FilterLink` is an effective for specifying
rewrite rules to be applied to the stream that the filter is filtering.

Problem is that this notion is very fine-grained. The filter can accept
one or more streams as input, apply a rewrite, and generate one or more
outputs. The inputs and the outputs are streams, but to specify the
"wiring diagram", in Atomese, those streams are necessarily anchored
at well-known locations (typically, under some key on some
`AnchorNode`.)

This makes processing with FilterNodes resemble an electronics circuit
diagram. Each "dot" in the cirucit is some Atom-with-Key location, that
must be explicitly specified. Each circuit element, spanning two or more
dots, is a `FilterLink` + `RuleLink` combo. The connection to a "dot"
is done with a `ValueOfLink` that explicitly names the dot.

Async elements, such as `QueueValue`s exist and work. This allows
readers to stall and wait for input. mux and demux are not an issue:
`QueueValue`s automatically demux, and a mux can be created with a
RuleLink that simply copies one input to two (or more) places.

Arbitrary processing DAG's can be built in this way. Running on the host
CPU. The proximal question is: can this be converted into a system for
wiring up flows on GPU's?

### Sections
Sections and connectors were originally invented to generalize the
concept of tensors, by allowing the specification of arbitrary
star-shaped patterns aka semi-assembled jigsaws. The types of the
connectors are loosened: instead of having just two types, "input"
or "output", there can be any number of types. The connection rules
are loosened as well: instead of assuming "heterosexual" rules, where
outputs must connect to inputs (only), one can have arbitrary mating
rules. Thus, a connection is allowed, if the mating rules allow it:
typically, the types must match, and the "sex" of the connectors must
be compatible.

As abstract mathermatics, this is a very powerful abstraction. As a
programming API, it is verbose and hard to work with. In particular,
a wiring engine is needed. Such a wiring engine has not yet been
created.

It is not particularly hard to specify a GPU kernel as a sheaf section,
but perhaps verbose. But that just specifies (describes) it. It also
needs to be wired in place. How?

Design Requirements
-------------------
For a prototype proof-of-concept demo, what's actually needed?

* *Basic vector I/O to GPU's.*
  Minimal function is to move data to GPU's, call kernel, get data back.

* *Multi-step dataflow wiring.*
  Define sophisticated data flow wiring. Similar to TensorFlow (???)

### Basic vector I/O
Ability to stream data data to GPU. Once-shot is a special case.
