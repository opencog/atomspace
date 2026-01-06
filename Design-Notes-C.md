Design Notes C
==============
Continuation of [Design Notes A](Design-Notes-A.md). January 2026

Applying functions to data
--------------------------
The three link types `ExecutionOutputLink`, `CollectionOfLink` and
`FilterLink` all apply functions to data. They were all born in
different eras, intended to solve different problems, using dramatically
different mechanisms to do so.  In retrospect, they seem to have more
in common, than they have differences, and so the question arises:
should the be merged into a grand one-size-fits all super-link that
does everything for everyone? Or is it better to keep them distinct?

A short history.
* The `ExecutionOutputLink` was designed specifically to call external
  functions (written in python, scheme, haskell or c++).  It was
  originally called `ExecutionLink`; the name was unilaterally changed,
  for the worse, I think.  It was a form of glue code: it unwrapped
  Atoms and presented argument lists in the format required by the
  foreign function.  One call, one set of arguments, one return value.
  An extremely conventional conception of a function call, embodied in
  Atomese.

* The `FilterLink` came next, replacing the now obsolete `MapLink`.
  The `MapLink` was inspired by scheme srfi-1 `map`, and similar ideas
  in haskell. The idea of `map` works in scheme, because "everything is
  a list".  It does not work in Atomese, because most things are not
  lists, and even when they are, the collection of items in a list can
  be highly non-uniform. It can be impossible to apply some functions
  on some inputs, and so the idea of srfi-1 `filter`, or even
  `filter-map` becomes more appropriate.  The function to be applied
  selects elements out of the list, or, if it is a RuleLink, applies
  a rewrite to the selected items.

The `FilterLink` mostly accepts most of the same kinds of functions
that `ExecutationOutput` does; except it does not (currently) handle
the `GroundedSchemaNode`s. As I write this, I cannot thiink of any
technical reason why these two could not be merged into one common Link.

* The `CollectionOfLink` is very recent; it was born of the idea that
  some types of lists (e.g. `ListValue`s) sometimes need to be rewritten
  into other kinds of lists (e.g. `ListLink` or `Setlink`). It was meant
  to be a quick cheesy hack utility, but has proven to be remarkably
  versatile, usable in situations outside its originally envisioned
  utility. But, as I write this, it begins to feel a bit like a
  `FilterLink` that can apply rules for type re-writting. Is it really
  needed as a distinct function?

### Base Questions
So the following questions arise:
* Are there technical constraints that prevent the merger of these
  three?
* Are there subtle semntic issues that blur the intended meaning of
  certain constructions?
* Are there usability issues that would make previously easy expressions
  pointlessly more complicated?

The above questions reveal complexities that have to be dealt with; on
closer examination, these links types are seen to be quite different
from one-another, and there's a fair amount of work needed to reconsile
these differences.

* `CollectionOf` wants to rewrite the *type* of collections.
* `FilterLink` defines a defacto guard, having the form of a pattern
  with 'variable' regions. These are not VariableNodes; they are
  anonymous and un-named. Thus, they cannot be bound for later
  rewriting.
* The `RuleLink`, when used with the `FilterLink`, does provide the
  rewriting.
* The `GroundedProcedureNode` is "by definition" without any type
  declarations.  The `ExecutionOutputLink` has no issue with this,
  but this presents a challenge for integration with `RuleLink`.
* What's the input? A static list? A stream? A container? Functions,
  guards, rewrites all tend to want to work one-item-at-a-time; there
  needs to be an input processor/unformizer that can take any source,
  and present items one-at-a-time.

### RuleLink
Lets review the current situation, and compare it to some notion of
desirable basics. The idea of mapping and filtering is somehow primal
to the processing of streams, and the `RuleLink` provides a basic frame
for defining a rewrite rule that can be applied to one item at a time.
But this is not how things started, and there are some cross-pressures.
So lets review `RuleLink` and its uses.

* The `RuleLink` was originally developed with an eye towards applying
  it in axiomatic reasoning, in the style of predicate logic, and of
  proof theory. Thus, as currently designed, it captures the form of
  `P(x)->Q(x)` or "if P(x) then Q(x)".
* The variable `x` is typed; the type decalaration follows the general
  Atomese conventions for typed variables.

### Critique of the URE
The `RuleLink` was developed for an intended use in the (now failed)
URE. A very very short review of the failure mode is in order. At first,
one imagines that it is easy to do inference and deduction: One writes
meta-rules that define how to combine rules. In practice, it is learned
that the proof-theoretic predicate-logic notation for rules is awful for
chaining. Outputs and inputs need to be compared for type agreement. If
inputs have variables, the variables must be quoted, as one does not
want to bind them, but merely check that the types are in agreement.
Quoting turns into a giant mess, as its a bit context dependent.

Another fundamental issue with the URE is the collision between
forward-chaining, backward-chaining and constraint satisfaction. When
using the proof-theoretic predicate-logic notation for rules,
forward-chaining is fairly easy: one uses an odometer-style generator,
and just exhaustively enumerates all possible inferences that satisfy
the type constraints. Backward chaining should be just as easy, except
for the assymmetry of rule specifications: viz the rules specify the
input type, but do not specify the output type.

The idea of chaining implies an odometer-style algorithm: one attempts
all possibilities. This is seen in Prolog; the `cut` is used to control
the explored paths. There are also constraint-satisfaction algos, such
as Answer Set Programming, which elevate the `cut` to a higher level.
The DPLL algo takes a "global" view of the collection of assertions,
then trims off trees, leaving behind a kernel which must then be
exhaustively explored (perhaps diagonalized, even, as kernels often
are.) The determination and location of cuts is autmated, reducing the
overall size of the satisfaction issue.

The idea of the Link Grammar jigsaw pieces is to remove the asymmetry of
the predicate-logic inference rules, and replace the notion of inputs
and outputs by typed connectors, and to completely remove variables from
the connector descriptions, thus providing a symmetrized jigsaw piece
that algos can work with: either the odometer aglo, or the ASP/DPLL
algo, or Viterbi algos, or the current LG algo implemented in the LG
code base, which does a fine job of "backward interence", matching the
connectors/disjuncts to words in a sentence. Any of these different
algos can be implemented to work with jigsaw pieces.

### Critique of Connectors/Sections
The concept of sheafs, sections and connectors appears to be ideal for
capturing the notion of a jigsaw piece: the connectors carry a type
marking, which, together with the connector sex, determines how
connectors can be mated.

This is fine for a connectionist description of semantic relationships
between symbols or semiotic signifiers. The connector types tell us what
can be related to what. As I write this, I imagine its not hard to
generalize connector mating to some kind of probabilistic strengths,
rather than a strict boolean true/false will-mate or won't-mate
determination. This probabilistic re-interpretation is defered for some
future project. ("Not hard" is an understatement: its probably quite
the adventure to do this, opening new vistas. Which is why it cannot be
attempted right here, just yet.)

This connectionist approach is lacking when the jigsaw pieces themselves
perform some sort of internal processing, e.g. when they are lambdas of
some sort, or functions, or relations. Here, variables are needed and
used so that the "wiring" on the inside of the lambda can be specified.
For example, in C++, java, python, scheme, the names of the variables
that appear as the inputs to functions are used in the body of the
function to connect into whatever algorithm or process is coded up
inside that function. Inputs that are named variables are ciritical to
convetional programming (for both the functional and procedural styles.)

But the jigsaw connectors, as currently defined in Atomese, do not
include Variable names. This is an unresolved design tension in Atomese.
To be explicit: we have
```
   (TypedVariable (VariableNode "$x") (TypeNode 'Concept))
```
or, with deep types:
```
   (TypedVariable (VariableNode "$x") (Signature ...)
```
What we want is
```
	(Connector
		(TypeNode 'Concept) ; or perhaps Signature, ...
		(SexNode "input")   ; the mating constraint
		(VariableNode "$x") ; the name assigned to the connector.
	)
```
The `TypedVariable` used in lambdas dispenses with the `SexNode`, such
connectors are always implcitly inputs to functions.

The constraint satisfaction solvers (cahiners, ASP, etc.) ignore the
name of the connector, as it is irrelevant for determining the typing
constraints.

The current wiki page for (Connector)[https://wiki.opencog.org/w/Connector]
lists the `Variable` as appearing first, and being optional. The
Connector is listed as an `OrderedLink`, but it might be better to have
it be an `UnorderedLink`?

### Pattern Analysis
Rules, in the form of `P(x)->Q(x)` have a natural interpretation as a
rewrite.  After isolating/grounding the variable `x`, extracting it from
the context `P(x)`, it is plugged into the context `Q(x)`.

The premise `P(x)` in its simplest form is just a pattern: a graph, with
some variable region identified as `x`. In the query engine, the
`PresentLink` is used to denote the pattern to be matched. The
`PresentLink` is itself an evaluatable predicate, evaluting to
true/false, depending on whether `P(a)` is present in the AtomSpace for
constant `a`. The invention of the PresentLink was spurred by the need
for the pattern matcher to also include other evaluatable terms;
originally "virtual" links like `GreaterThanLink` but later any kind of
evaluatable clause that evaluates to boolean true/false.

The `GreaterThanLink` was called "virtual" because it was not "actually"
present in the AtomSpace: there is an infinite number of greater-than
relations between the integers, and they cannot all be stored in the
AtomSpace. The `GreaterThanLink` is a stand-in for a relation on
infinite sets. The set itself is constrained by the type defintions
on the "input" variables: `greater(x,y)` treats `x` and `y` as "inputs",
consrained as (for example)
```
	(TypedVariable (Variable "$x") (TypeNode 'Number))
```
so that `(Type 'Number)` is both a type, and a stand-in for the semantic
idea of an infinite set of numbers.

Such virtual links can be any generic relation:
```
	(Lambda
		(VariableList (Variable "$x") (Variable "$y"))
		(body that evaluates to a BoolValue))
```
There are two basic link types in the query engine: the `QueryLink`,
which corresponds to the implcation aka rewrite `P(x)->Q(x)` and the
`MeetLink`, which corresponds to `P(x)->x`. Although one can think of
`x` as a special case of `Q(x)` it is more convenient to just have a
post-processing rewrite `x->Q(x)` that, given a grounding `x:=g` will
instantiate `Q(g)` from the provided template `Q(x)`.

Thus, the final generic form for `P(x)` is what it has always been for
the query engine: it is of the form
```
	(VariableList
		(TypedVariable (Variable "$x") (... type spec ...))
		(TypedVariable (Variable "$y") (... type spec ...))
		... etc.)
	(AndLink
		(PresentLink (tree with x,y,z.. in it))
		(PresentLink (different tree with x,y,z.. in it))
		(predicate relation that evaluatoes to true/false)
		(another predicate relation that evaluatoes to true/false)
		... etc.)
```
The `PatternLink` performs query analysis on the above form, extracting
the variables, the relations, computing the connectivity graph between
the variables appearing in the various clauses, passing quotations
correctly, etc. and it is specifically tuned for pattern analysis for
the query engine.

The `QueryLink` and `MeetLink` both inherit from `PatternLink`. They
apply to the whole of the AtomSpace. They are not applied to elements,
one-at-a-time, arriving via `FilterLink`.

### GuardLink
The current `FilterLink` does not avail itself of this analysis. Or
rather,


guards as connection
rules as rewrte
as flows
rewrite
output type

PatternLink
GuardLink

### TODO
* Fix Connectors so that they can be named and typed as expected.
  (Fix?? Where? What?)
* Should COnnectors be Unordered??
* Should we formalize the idea of a BoolLambda?
