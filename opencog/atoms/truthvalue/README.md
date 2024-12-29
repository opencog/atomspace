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
substrate for teh ideas in this book, and so truth values became deeply
embedded in the design.

This was OK, given the era, and that style of thinking. But life moves
on, and we grow older and wiser. The original conception for TruthValues
doesn't quite work out. That's OK, but the injury comes from the C++
implementation: carrying around TruthValues became problematic. The
harsh reality is that most "real life" calculations to be done in the
AtomSpace require crisp boolean true/false values. Using TruthValues
just gets in the way, hurting performance and glopping up the API.

### Criticism
