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
rather, the analysis provided by `PatternLink` is custom-tailored for
graph walks of the entire AtomSpace, and not for element processing.

The current `RuleLink` does do this, but (in it's current form) is
minimalist: It will accept arbitrary `VariableList` variable
declarations, but effectively has only a single `PresentLink` as the
stand-in for the premise `P(x)`.

In analogy to `MeetLink`, I think we want to define a `GuardLink`, of
the form
```
	(GuardLink
		(VariableList ...)
		(Present ...) ; only one is possible; or a ChoiceLink
		(And
			(... zero or more evaluatable predicates...)))
```
such that, when executed, it indicates whether the `PresentLink` is
satisfiable by the provided "input" item (thus acting as a filter),
then evaluating the predicates (which must all evaluate to true). The
result of execution must be not only the true/false of satisfiability,
but also the specific groundings of the variables, so that these can be
used in the rewrite (instantiation) `x->Q(x)` part of the rule.

Note that (very unlike `PatternLink`) the above can only have *one*
`PresentLink`, as the input item is presumed to be a specific tree.
(viz just one `Link` or `Value`) Of course, the `ChoiceLink` can stand
in the place of the `PresentLink`, to provide a choice of clauses to
match against. The point is there must be only one clause, in the end.

With some squinting, though, the multi-clause structure of the
`PatternLink` can be thought of as a single clause, the unordered set
`AndLink` of sub-clauses, all of which must appear in the input stream,
where each element of the input stream is a `SetLink` that holds a
bundle of subclauses against which the pattern is compared. (As written
it has to be a `SetLink`, the `LinkValue` is ordered, and we don't
currently have an `UnorderedValue` that would be required here.)

The wiki page for [RuleLink](https://wiki.opencog.org/w/RuleLink) does
give the form
```
	(RuleLink
		(VariableList ...)
		(And
			(premises 1)
			...
			(premise K))
		(conclusion 1)
		...
		(conclusion N))
```
so perhaps we should accept that the `GuardLink` can be just the first
half of that, and that, in practice, when used for filtering, only one
of the premises will be a `PresentLink`, and the rest will be
evaluatables.

### Typing Equivariance
Historically, typing constraints were designed to be part of the
vardecl, specified with `TypedVariableLink`. Why? Because that is
how C++ and Java do it: these languages put thier type constraints
in the function declaration. But here, in Atomese, we have more
freedom, and can move the type constraints into the body, as
predicates: e.g.
```
	(Rule
		(Variable "$x")
		(And
			(Equal (TypeOf (Variable "$x") (Type 'Concept)))
			(Present ...))
		(conclusion ...))
```
The above appears to have the same meaning as the below:
```
	(Rule
		(TypedVariable (Variable "$x") (Type 'Concept))
		(Present ...)
		(conclusion ...))
```
These two forms are equivariant. Are they "semantically equivalent",
and in what sense? They seem to be very nearly equivalent from the
stream processing point of view. The second form might be slightly more
efficient for the algo doing the processing, maybe. If there is some
pattern analysis to be applied "at compile time", the second form has
clear advantages (see the implementation of `PatternLink` as a concrete
example.)

However, from the connectionist form, where some rule engine will be
determining how connectors can mate, the second form has huge
advantages. It clearly cannot be mated to any output connector that does
generate ConceptNodes, or a super-type of Concepts. The first form will
mate to any type. If we are building up data processing networks with
a rule engine, this would be useless, if this input was connected to
something that produces e.g. only Predicates.

This is why e.g. c++ and java have type restrictions in the function
declarations; they can be analyzed at compile time. CaML is the extreme
case. Python claims "lazy typing", making it easier for the human, at
some runtime penalty. Python was inspired by Lisp, which attempted to
avoid typing, but finds it impossible to evade, in the end. For Atomese,
we can have it both ways, it would seem. And since they are equivariant,
we can even hoist EqualLinks out of the body and into type declarations.

But this is very hand-wavey. In the above example, the `EqualLink` has a
very specific form. How much, exactly, can be hoisted? Is this like the
duality between lambdas and combinators, which are in one-to-one
correspondence? Or is the collection of possible rewrites much more
limited?

Combinators were rewriting lambda function bodies, whole-sale. Here. we
are rewriting into types. Can we rewrite function bodies whole-sale,
into types? This seems impossible. I'm confused. Wtf.

Defining a rewrite rule that will transform the one into the other would
be an interesting challenge, as it would stress the ability to perform
such homotopic rewrites. The syntactic issues needed to specify the
rewrite would pose considerable challanges. The URE used QuoteLinks
extensively to do this; I doubt that this was the correct or wise way
of doing this.

This extends to more complex Signature types. So, for example:
```
	(Rule
		(VariableList (Variable "$x") (Variable "$y"))
		(Present (Variable "$y"))
		(Equal
			(Variable "$y")
			(Edge (Predicate "foo")
				(List (Variable "$x") (Concpet "bar"))))
		...)
```
can be written as
```
	(Rule
		(TypedVariable (Variable "$y")
			(Signature
				(Edge (Predicate "foo")
					(List (TypeNode 'Atom) (Concpet "bar")))))
		(Present (Variable "$y"))
		...)
```
where the named but unused `(Variable "$x")` is replaced by the
anonymous `(TypeNode 'Atom)`.

### Connectivity
The `RuleLink` as specified above presents a challenge to the
connectionist approach to constraint satifaction. The sheaf design
expects the form
```
	(ConnectorSeq
		(Connector
			(Variable "$x")
			(Type 'Concept)
			(Sex "input"))
		...
		(Connector
			(Type ...)
			(Sex "output")))
```
The `VariableList` is obviously a `ConnectorSeq` with implicit input
`SexNode`s. What's missing in the `RuleLink` is any description of the
output connector. That because in most applications we don't care; just
run the filter, and whatever comes out, comes out. That is, there is an
implicit
```
	(Connector
		(Type 'Value)
		(Sex "output"))
```
present in every `RuleLink`. This connector is anonymous. Now comes
something to trip across. If we want to name it, should we write
```
	(Connector
		(NameNode "my favorite output port")
		(Type 'Value)
		(Sex "output"))
```
or
```
	(Connector
		(VariableNode "my favorite output port")
		(Type 'Value)
		(Sex "output"))
```
As explained earlier, the intent of `NameNode` is to name specific data
streams, and not to name output ports. But it is handy to have the
`NameNode` be distinct, because it serves the "opposite" purpose: the
named variables are used to hook up internal wiring diagrams, inside of
lambdas, while `NameNodes` can then be used exclusively for the external
wiring, between lambdas.

This seems to make sense and to be intuitive, as long as there are two
sexes, "input" and "output". More than two (e.g. left/right, by
head/dependent, vowel/consonant, etc.) start to create issues, which we
avoid for now. Suffice to say that some sexes, e.g. vowel/consonant, can
also be encoded by bond subtypes (i.e. how Link Grammar currently does
this for English a/an phonemes.) Again, there seem to be equivariant
representations. Sex and Bond are the most conveniant for the simple
cases, but don't generalize (easily).

### Rewriting
A short review of the history of the `RuleLink` is in order.

The URE was an attempt to implement an inference engine for realizing
PLN. It was hoped that the URE would be usable for general rewriting,
too, but that never materialized. Part of the issue is driven by
confusion as to what a rule is, and what a rewrite is. Additional
confusion is driven by the demands of various different users and thier
expectations for what the API would be. The rule engine needed to
implement purchase orders for office-chair requistions routed through
accounting, management, shipping&receiving has broad similarities to the
rule engine inside of the RelEx relationship extractor; inventing an API
suitable for both requires serious work that was never undertaken.

The original definition of PLN was reimagined to be a probilistic form
of [natural deduction](https://en.wikipedia.org/wiki/Natural_deduction).

For reasons unclear to me as I write this, the implementation required
function composition as a basic inference rule. It was realized as beta
reduction, with all the trimmings.  For example, using Gentzen tree
notation, one might have an expression of the form
```
    P(x)->Q(x) ,  x=A(y)
    -----------------------
           Q(A(y))
```
The representation of `P(x)->Q(x)` is as written earlier.
```
	(RuleLink
		(VariableList ... x ....)
		(P ... x ...)
		(Q ... x ...))
```
This is already written in prenex form, so that the vardecls come first,
and any other variables that appear in `P` or `Q` are "block scope", and
are not externalized as the "input connectors" of the rule.

The `A(y)` has the representation
```
	(Lambda
		(VariableList ... y ...)
		(A ... y ...))
```
and the trick is to create `Q(A(y))` also having this lambda form. That
means disassembling `Q(x)` into its vardecls and body, and also
disassembling `A(y)`, plugging the body of `A` into the slots `x` of
`Q` (i.e. "beta-reducing"), then performing any additional reducts that
might be available to the combined expression, then re-assembling a
brand-new lambda, having `y` as the vardecls, and the `QA` reduction as
the body. The vardecls for `x` are discarded.

The vardecls are managed by `ScopeLink`; this is the base class for both
`LambdaLink` and others. The distinction is that `ScopeLink` only deals
with variable binding, while `LambdaLink` is reserved for functions. For
example, the expression "`forall x, P(x)`" binds the variable `x`, but
it is not a function, and thus not a lambda. So, `ForAllLink` inherits
from `ScopeLink`, not `LambdaLink`.

The dis-assembly/re-assembly of `Q` and `A` is managed by `RewriteLink`;
it inherits from `ScopeLink` since the management of the vardecls is an
intimate part of the rewriting.

Some rewrites have the form
```
    P(x)->Q(x,y) ,  x=A(z)
    -----------------------
           QA(z,y))
```
This requires disassmebling the vardecl for `Q(x,y)`, and pulling out
the vardecl for `z` out of `A(z)`, and then assembling a new vardecl
`(z,y)`. This disassembly-reassembly brings the final expression back
into prenex form.

The original `RewriteLink` is not sophisticated enough to perform the
required re-assembly (rewrite) of the vardecls; the `PrenexLink`
provides this.

To summarize: `RuleLink` inherits from `PrenexLink`. `Prenex` inherits
from `RewriteLink`; this inherits from `ScopeLink`.

### Proof-theoretic muddle
Note the complex recursive nature of the explanations above. We start by
imaginging Gentzen tree notation to be an "inference rule", but are
promptly lead to discussions of re-writes applied to vardecls. Worse,
these re-writes are ad-hoc, implemented in c++ code, and are NOT overtly
expressed in Atomese.

More generally, we have a muddle, partly intentional, and partly
accidental, as to what a "rule" is, in the first place. Is it an
inference rule? Well, yes. Is it an axiom schema? Well, that too.

This has consequences. Using RuleLinks for both axiom schemas and also
for inference rules requires `QuoteLink` to be conjured up. Why? When an
inference rule is applied to an axiom schema, the schema is necessaily
an un-evaluated literal, a constant expression; only the inference rule
itself is being evaluated. But since these both "look alike", one must
be quoted to prevent it's evaluation.  This is a valid way of going
about things, but dramaitcally increases the complexity of the inference
rules.

The rewrite of vardecls provides a concrete example. If we have, as
above, the expressions
```
	(Fiddle
		(VariableList (Variable "x") (Variable "y"))
		(Stuff))
```
and
```
	(Faddle
		(Variable "z")
		(Glop))
```
and desire the above to be rewritten into
```
	(VariableList (Variable "z") (Variable "y"))
```
then how, exactly, do we write the `RuleLink` that specifies this
rewrite? It can be done, but I don't want to even try; its complicated.

To the extent that it turns into a mess will indicate a failure of the
Atomese infrastructure for graph rewriting. This is an exercise that
must be eventually undertaken, but ... not yet.




ScopeLink handles vardecl only. GuardLink can inherit from Scope.
Rewrite can inherit from Guard.




simplest derevied classes from RuleLink so to be modular design ...

The need to evaluate guard when doing connectionist rules.

The fact that guard looks like a connectorSeq with lots of extra
complexity. ..

guards as connection
rules as rewrite
as flows

GuardLink
Prenex, rule fro guard

### TODO
* Fix Connectors so that they can be named and typed as expected.
  (Fix?? Where? What?)
* Should COnnectors be Unordered??
* Should we formalize the idea of a BoolLambda?
