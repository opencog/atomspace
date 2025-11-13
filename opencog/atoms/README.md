
Atoms
=====

This directory contains C++ implementations of many (but not all)
atom types as C++ classes (some atom types do not need an underlying
C++ class to make them work).

The goal of having a C++ class is two-fold. One is to implement methods
that "do something" for that atom type. For example, the `PlusLink`
can actually add numbers together, as well as holding the symbolic
expressions to be added.  Another goal of having C++ classes is that
they allow caching, pre-processing and memoization. For example, the
pattern matcher needs to know where all the variables are in a pattern;
the `PatternLink` pre-analyzes the pattern and memoizes the variable
locations. The `PatternLink` holds the "compiled" version of the
pattern.

See the [README](../README.md) one level down for a quick definition
of what an Atom is.

See the [AtomSpace README](../atomspace/README.md) for a discussion of
design tradeoffs, and of the current implementation.

Subdirectories
--------------
Listed in order of dependency:

 * `atom_types` -- The `atom_types.script` file in this directory
   declares the `Value` and `Atom` type hierarchy. Includes the
   `nameserver`, which provides misc run-time services, describing
   the type hierarchy and the names of types.

 * `value` -- The C++ base class for Atoms and for Values. The
   `FloatValue`, `StringValue` and `LinkValue` are defined here.

 * `base` -- The C++ base classes for `Atom`, `Node` and `Link` are
   defined here. The Atom factory (aka `classserver`) is defined here.

 * `core` -- A large collection of Atoms that have "fairly simple"
   C++ classes behind them.  Here, "fairly simple" means that they do
   not depend on other C++ subsystems, such as scheme, python, the
   pattern matcher, etc.

 * `flow` -- Atoms that move (get/set) Values from/to Atoms.

 * `pattern` -- Atoms involved with searching for pattern: `QueryLink`,
   `MeetLink`, `DualLink`, `SatisfactionLink`, `SatisfactionSetLink`.
   These are all quite complicated, and cache various pre-compiled
   parts of the pattern.

 * `join` -- Similar to the pattern queries, but these look
   "upwards" instead of "downwards". Thus, `JoinLink` is here
   (contrasted to `MeetLink` above)

 * `execution` -- miscellaneous executable/evaluatable atoms. Much of
   the code here should probably migrate to per-atom-type C++ classes.

 * `parallel` -- Links types that can launch multiple threads (and join
   them again) when executed.

 * `grounded` -- "black-box" executable/evaluatable nodes, that is,
   `GroundedPredicateNode` and `GroundedSchemaNode`.  These interact
    with the scheme, python and haskell evaluators.

 * `reduct` -- inspired by comboreduct, these are "clearbox" links:
   `PlusLink`, `TimesLink`, etc. They not only represent arithmetic
   formulas, but can also add and multiply numbers.  They are
   "clearbox" because they not only "do something" (like the black-box
   Atoms), but we also know what they do.  For example, `PlusLink` adds
   numbers.  By contrast, it is impossible to know what some black-box
   might do. Thus, we can apply reasoning and deduction to the clearbox
   links, which is impossible for the black-box links.

 * `foreign` -- an experiment in its early stages to map arbitrary
   languages into the atomspace. That is, the AtomSpace stores trees,
   and the source code of pretty much all programming languages can be
   converted into abstract syntax trees (AST's). These trees can be
   stored in the AtomSpace. All that's missing is some pretty-printing:
   wrappers to convert those trees into AtomSpace trees, and the
   converse: print them back out again, in the expected format.

TruthValues
===========
TruthValues are now gone.

TruthValues, along with Atoms, are the oldest concepts in Atomese.
They were an important, even central concept to the idea of the
AtomSpace. Yet, the original vision was problematic and ultimately
unsupportable. Not to worry: they have been replaced by something
even better, and more general: Values. In particular, FloatValue
is the most immediate, direct successor. Below is a review of the
original concept, a critique of what went wrong, and why. And some
ideas on how one moves forward.

### History
The idea of a "truth value" is central to logic, and is one of the
primary topics of any introductory textbook on predicate logic.
(Dirk van Valen, "Logic and Structure", Springer is recommended).

The earliest design of Atomese realized that the crisp true-false
of classical predicate logic needed to be generalized to floating
point numbers, in part to allow calculations with "fuzzy logic",
and in part to allow the application of concepts from probability
theory.

Rather than specifying truth values as a single float, two were
defined: the "strength" and the "confidence". This is the so-called
`SimpleTruthValue`. The strength would represent how true (or untrue)
some given inference is, and the confidence would indicate the confidence
in that result.

Ben Goertzel's book "Probabilistic Logic Networks" spells out exactly how
this was to work: various types of inference, deduction, extrapolation
and so on would be done, with specific, explicit formulas giving the
truth value update. The original AtomSpace was designed to be the
substrate for the ideas in this book, and so truth values became deeply
embedded in the design.

This was OK, given the era, that style of thinking, and the available
compute power. But life moves on, and we grow older and wiser. The
original conception for TruthValues doesn't quite work out. That's OK,
but the injury comes from the C++ implementation: carrying around
TruthValues became problematic. The harsh reality is that most "real
life" calculations to be done in the AtomSpace require crisp boolean
true/false values. Using TruthValues just gets in the way, hurting
performance and glopping up the API.

### Critique
Two critiques of TruthValues can be given; one conceptual, and one from
a software development point of view. The software critique can't really
be understood until the conceptual issue is articulated.

### The early days
Even as the PLN book is being written (in the 1990's) it is clear that
"naive" probabilistic inference rapidly accumulates uncertainties, which
fairly immediately wash out the confidence in any result, or drive the
expectation value of truth to one-half. One work-around in the book is
to propose upper and lower bounds, which act like error bars, modelling
an asymmetric but still central-value-ish, Bell-curve-like peaked
distribution. This drives TruthValues from two floats, to four.

One can also be creative, and think about experimenting with fuzzy truth
values. This was a thing in the 1970's and 1980's, and kind of made
sense, given the speed of CPU's in that era, and the dawning realization
that crisp true-false can't do it all.  So Atomese sprouts a
FuzzyTruthValue.

Trouble with mapping Bayesian inference to TruthValues (which only
record the expectation value, and not the whole distribution) results
in a DistributionalTruthValue being invented. Its a vector of floats.

Problems with merging, handling and combining these truth values arise.
How should inference formulas be handled? Where should the arithmetic be
done?

### Generalization to Values
Eventually, several general ideas become clear. First and most
obviously, that a generic vector of floats is sufficient to record the
assorted floating point data that the various TruthValues need.
Second is that the update formulas need to be moved out of the C++
implementation, and represented as actual Atomese arithmetic formulas.
Third is that having valuations of things other than floats is
interesting. This includes both strings and trees, opening up the
possibility of having valuations that are not just concrete numbers,
but abstract objects.

### Bayesian Inference
Finally, and most importantly, and perhaps the most difficult to
understand is that working with probabilistic inference applied to
expectation values is just plain wrong. This can be stated in two
different ways.

First, Bayesian inference performs an **integral** on the priors.
Integrals are "infinite-dimensional" by definition, but can be
approximated by histograms with narrow bins: this is just the
classic Newtonian integral. This can be managed with a vector of
floats, at least in principle. In practice, a number of distinct
issues arise.

* Large vectors require a lot of storage and a lot of compute to
  update.
* The issue of distributinos "washing out" after only a handful of
  inferences remains.
* When distributions converge, they do so only slowly.
* Inference must be performed not on just a small handful of
  relationships between objects, but on large, even huge networks
  of inter-related structures.
* The larger the network becomes, the slower the convergence.

### Neural Nets
The second way of thinking about Bayesian inference is that this is what
deep-learning neural nets do. Even as Atomese is developing from its
roots in symbolic AI, the technology for neural nets is rapidly
advancing. The issue of the vastness of the space of Bayesian priors,
and the vastness of the interconnections and relations between items is
tackled head-on.

Inference is performed not just on two or three "inputs" as they are in
PLN, but on hundreds, at first, and then thousands, and now millions. Or
more. I can't keep track of the state of the art.  Custom hardware, aka
GPU's, allow the arithmetic operations to be distributed.

The half-dozen or dozen update formulas of PLN are replaced by just one
(or two, or three...) in neural nets: sum the inputs, run them through a
sigmoid. The old PLN rules almost-kind-of sort-of-maybe did this, but
without realizing that this is what they were doing. And without
realizing that the doors could be opened to more than just two or
three inputs.

There is a struggle to develop the idea of back-prop in PLN; it is
called the "backward chainer" in the URE. However, the idea that it
needs to be very small, very simple, and very fast is never realized:
instead of measuring performance and reducing the size, the software
complexity of the URE grows without bounds, even as performance degrades
boundlessly.  This is, of course, fatal.

### A Defense of Symbolic AI in a Neural Net World.
A PLN-inspired approach to high-dimensional spaces, competitive with the
achievements of neural nets, is still possible. There are a least two
distinct approaches. We'll have to invent some fanciful names for them.

* The ant colony. In this scenario, all "truth value" updates continue
  to be made in a very high-dimensional space, but instead of working
  directly with those vectors, only a small number of inputs are sampled
  and combined. These produce an update, which is used to revise the
  representation vector. The vector becomes a dynamical object, but
  only a low-dimensional subspace is updated at any one time.  This
  hews closely to the original conception of PLN.

  The differences require a change of mindset. The performance can't be
  sloppy: updates must happen in microseconds, and must be distributed
  onto GPU's. The theory can't be sloppy: one should never pretend that
  one is updating expectation values, but rather that one is emulating
  a microcanonical ensemble. In solids, liquids and gasses, interactions
  are pair-wise; these give rise to the bulk properties. Similarly here:
  interactions can be small, feeble, ant-like, but those interactions
  factorize the probability distribution the same way that perturbation
  theory factorizes particle interactions.  Perhaps a different example:
  the Ising model uses pairwise interactions. The Ising model doesn't
  use a million-by-million dimensional interaction matrix. Yet its quite
  rich in it's results. So too could an ant-colony of small-scale but
  fast, frequent updates.

  Or so that's the sketch. Of course, this is an idea that has almost no
  theoretical development and is kind of completely passed over. So it
  goes.

* The flow network. A simplistic form of this is already implemented in
  the mainstream, in systems like TensorFlow. A graphical diagram is
  created, indicating how million-dimensional vectors should be updated.
  The graph is compiled down to code, and run on GPU's. The result maybe
  works and maybe doesn't -- you'll need an nvida card to find out.

  The flow networks are represented in Atomese, and are modified and
  updated in various unspecified ways, thanks to algorithmic control
  over Atomese. Pie in the sky.  The things that "flow" are still truth
  values; just that now they are called "representation vectors";
  they live and are updated on GPU's, while Atomese is used to apply
  some additional massaging to these vectors. An obvious example
  might be to apply some form of old-school symbolic logic to push the
  vectors of "king and queen" close to the vectors for "male and
  female".

  In this conception, networks in the atomspace not only represent flows
  that update weights, but alsmo map a symbolic representational network
  on top of the high-dimensional representation vectors, allowing a form
  of classical inference that pure LLM's do not provide. Or something
  like that. Again, there is approximately zero theoretical development
  to support this application, and the idea is roundly ignored in all
  quarters. The human anthill is not toiling at this particular
  coal-face.

TruthValue Redux
----------------
As of 2025, both of the above ideas are being pursued. This is happening
mostly outside of this git repo; they appear in other related projets,
including `sensory` and `atomese-simd`.
