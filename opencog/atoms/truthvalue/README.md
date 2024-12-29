TruthValues
-----------
TruthValues, along with Atoms, are the oldest concepts in Atomese. They
are now little-used, and are long overdue for a major design overhaul.
Some history, and some criticism is in order.

### History
The idea of a "truth value" is central to logic, and is one of the
primary topics of any introductory textbook on predicate logic.
(Dirk van Valen, "Logic and Structure", Springer is recommended).

The earliest design of Atomese realized that the crisp true-false
of classical predicate logic needed to be generalized to floating
point numbers, in part to allow calculations with "fuzzy logic",
and in part to allow the application of concept from probability
theory.

Rather than specifying truth values as a single float, two were
defined: the "strength" and the "confidence". This is the so-called
`SimpleTruthValue`. The strength would represent how true (or untrue)
some given inference is, and the confidence woud indicate the confidence
in that result.

Ben Goertzel's book "Probabiistic Logic Networks" spells out exactly how
this was to work: various types of inference, deduction, extrapolation
and so one would be done, with specific, explict formulas giving the
truth value update. The original AtomSpace was designed to be the
substrate for the ideas in this book, and so truth values became deeply
embedded in the design.

This was OK, given the era, and that style of thinking. But life moves
on, and we grow older and wiser. The original conception for TruthValues
doesn't quite work out. That's OK, but the injury comes from the C++
implementation: carrying around TruthValues became problematic. The
harsh reality is that most "real life" calculations to be done in the
AtomSpace require crisp boolean true/false values. Using TruthValues
just gets in the way, hurting performance and glopping up the API.

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
an asymmetric but still central-value-ish, Bell-curve-like peaks
distribution. This drives TruthValues from two floats, to four.

One can also be creative, and think about experimenting with fuzzy truth
values. This was a thing in the 1970's and 1980's, and kind of made
sense, given the speed of CPU's in that era, and the dawning realization
that crisp true-valse can't do it all.  So Atomese sprouts a
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
approximated by historgrams with narrow bins: this is just the
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
sigmoid. The old PLN rules almost-kind-of soft-of-maybe did this, but
wthout realizing that this is what they were doing. And without
realizing that the doors could be opened to more than just two or
three inputs.

There is a struggle to develop the idea of back-prop in PLN; it is
called the "backward chainer" in the URE. However, the idea that it
needs to be very small, very simple, and very fast is never realized:
instead of measuring performance and reducing the size, the software
complexity of the URE grows without bounds, even as performance degrades
boundlessly.  This is, of course, fatal.



Most
The original PLN, conceived of performing calculations on expectation
values, was fine, I guess, for the 1980's in which it is conceived, back
when

Even as this becomes clear in the 
