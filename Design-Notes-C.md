Design Notes C - GuardLink
==========================
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
  functions (written in python, scheme, Haskell or c++).  It was
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
that `ExecutionOutput` does; except it does not (currently) handle
the `GroundedSchemaNode`s. As I write this, I cannot think of any
technical reason why these two could not be merged into one common Link.

* The `CollectionOfLink` is very recent; it was born of the idea that
  some types of lists (e.g. `ListValue`s) sometimes need to be rewritten
  into other kinds of lists (e.g. `ListLink` or `Setlink`). It was meant
  to be a quick cheesy hack utility, but has proven to be remarkably
  versatile, usable in situations outside its originally envisioned
  utility. But, as I write this, it begins to feel a bit like a
  `FilterLink` that can apply rules for type re-writing. Is it really
  needed as a distinct function?

### Base Questions
So the following questions arise:
* Are there technical constraints that prevent the merger of these
  three?
* Are there subtle semantic issues that blur the intended meaning of
  certain constructions?
* Are there usability issues that would make previously easy expressions
  pointlessly more complicated?

The above questions reveal complexities that have to be dealt with; on
closer examination, these links types are seen to be quite different
from one-another, and there's a fair amount of work needed to reconcile
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
  needs to be an input processor/uniformizer that can take any source,
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
* The variable `x` is typed; the type declaration follows the general
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
for the asymmetry of rule specifications: viz the rules specify the
input type, but do not specify the output type.

The idea of chaining implies an odometer-style algorithm: one attempts
all possibilities. This is seen in Prolog; the `cut` is used to control
the explored paths. There are also constraint-satisfaction algos, such
as Answer Set Programming, which elevate the `cut` to a higher level.
The DPLL algo takes a "global" view of the collection of assertions,
then trims off trees, leaving behind a kernel which must then be
exhaustively explored (perhaps diagonalized, even, as kernels often
are.) The determination and location of cuts is automated, reducing the
overall size of the satisfaction issue.

The idea of the Link Grammar jigsaw pieces is to remove the asymmetry of
the predicate-logic inference rules, and replace the notion of inputs
and outputs by typed connectors, and to completely remove variables from
the connector descriptions, thus providing a symmetrized jigsaw piece
that algos can work with: either the odometer aglo, or the ASP/DPLL
algo, or Viterbi algos, or the current LG algo implemented in the LG
code base, which does a fine job of "backward inference", matching the
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
determination. This probabilistic re-interpretation is deferred for some
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
inside that function. Inputs that are named variables are critical to
conventional programming (for both the functional and procedural styles.)

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
connectors are always implicitly inputs to functions.

The constraint satisfaction solvers (chainers, ASP, etc.) ignore the
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
`PresentLink` is itself an evaluatable predicate, evaluating to
true/false, depending on whether `P(a)` is present in the AtomSpace for
constant `a`. The invention of the PresentLink was spurred by the need
for the pattern matcher to also include other evaluatable terms;
originally "virtual" links like `GreaterThanLink` but later any kind of
evaluatable clause that evaluates to boolean true/false.

The `GreaterThanLink` was called "virtual" because it was not "actually"
present in the AtomSpace: there is an infinite number of greater-than
relations between the integers, and they cannot all be stored in the
AtomSpace. The `GreaterThanLink` is a stand-in for a relation on
infinite sets. The set itself is constrained by the type definitions
on the "input" variables: `greater(x,y)` treats `x` and `y` as "inputs",
constrained as (for example)
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
which corresponds to the implication aka rewrite `P(x)->Q(x)` and the
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
		(predicate relation that evaluates to true/false)
		(another predicate relation that evaluates to true/false)
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
how C++ and Java do it: these languages put their type constraints
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
rewrite would pose considerable challenges. The URE used QuoteLinks
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
				(List (Variable "$x") (Concept "bar"))))
		...)
```
can be written as
```
	(Rule
		(TypedVariable (Variable "$y")
			(Signature
				(Edge (Predicate "foo")
					(List (TypeNode 'Atom) (Concept "bar")))))
		(Present (Variable "$y"))
		...)
```
where the named but unused `(Variable "$x")` is replaced by the
anonymous `(TypeNode 'Atom)`.

### Connectivity
The `RuleLink` as specified above presents a challenge to the
connectionist approach to constraint satisfaction. The sheaf design
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
representations. Sex and Bond are the most convenient for the simple
cases, but don't generalize (easily).

### Rewriting
A short review of the history of the `RuleLink` is in order.

The URE was an attempt to implement an inference engine for realizing
PLN. It was hoped that the URE would be usable for general rewriting,
too, but that never materialized. Part of the issue is driven by
confusion as to what a rule is, and what a rewrite is. Additional
confusion is driven by the demands of various different users and their
expectations for what the API would be. The rule engine needed to
implement purchase orders for office-chair requisitions routed through
accounting, management, shipping&receiving has broad similarities to the
rule engine inside of the RelEx relationship extractor; inventing an API
suitable for both requires serious work that was never undertaken.

The original definition of PLN was re-imagined to be a probabilistic form
of [natural deduction](https://en.wikipedia.org/wiki/Natural_deduction).

Consider the following inference rule, written in Gentzen notation:
```
    P(x)->Q(x) ,  Q(y)->R(y)
    ------------------------
           P(z)->R(z)
```
Using sequent calculus notation, it would be written as
```
	P(z)->R(z) |- P(x)->Q(x) ,  Q(y)->R(y);
```
This is more-or-less Prolog notation or ASP (answer set programming)
notation.  The variables `x`, `y`, `z` might be single variables, or
they might be sets of many variables.

For the present discussion, it is useful to write the implication using
the `ImplicationLink`. Thus, the representation of `P(x)->Q(x)` is now
written much like before, but we use a different link type:
```
	(ImplicationLink
		(VariableList ... (Variable "x") ....)
		(P ... (Variable "x") ...)
		(Q ... (Variable "x") ...))
```
Notice that this is in prenex form, so that the vardecls come first. If
there are any other variables that appear in `P` or `Q`, they are "block
scope", and are not externalized as the "input connectors" of the link.

How is this to be represented in Atomese? Well, apparently as
```
	(RuleLink
		(VariableList
			(Variable "$vardecl-x")
			(Variable "$vardecl-y")
			(Variable "$P")
			(Variable "$Q")
			(Variable "$R"))
		(And
			(LocalQuote
				(Implication
					(Variable "$vardecl-x")
					(Variable "$P")
					(Variable "$Q")))
			(LocalQuote
				(Implication
					(Variable "$vardecl-y")
					(Variable "$Q")
					(Variable "$R"))))
		(LocalQuote
			(Implication
				(Variable "$vardecl-x")
				(Variable "$P")
				(Variable "$R"))))
```
The `AndLink` says that there are two premises, to be combined. The
`LocalQuoteLink` says that each part is a literal, and not to be
interpreted.  The deluge of `Variables` are used to decompose the
inputs into their component parts.

In the above, it might have been more correct to use `PresentLink`
instead of `LocalQuote`, but historically, the `LocalQuote` got used.

The variable declarations probably should have been written as
```
	(RuleLink
		(VariableList
			(TypedVariable (Variable "$vardecl-x")
				(TypeChoice
					(Type 'Variable)
					(Type 'TypedVariable)
					(Type 'VariableList)))

			(TypedVariable (Variable "$vardecl-y")
				(TypeChoice
					(Type 'Variable)
					(Type 'TypedVariable)
					(Type 'VariableList)))

			(TypedVariable (Variable "$P") (Type 'Link))
			(TypedVariable (Variable "$Q") (Type 'Link))
			(TypedVariable (Variable "$R") (Type 'Atom)))
	...)
```
This asserts that in the decomposition into parts, the vardecls really
are vardecls, and not just blobs. The `TypeChoice` just says that we
can match vardecls in any of several forms.

Note that the above acts as a guard: if the expression does not match,
then the rule cannot be applied. This motivates the development of the
`GuardLink` as a base class. (The `GuardLink` does not exist yet... but
it will, soon(?).)

The vardecls are managed by `ScopeLink`; this is the base class for
`RuleLink`, `LambdaLink` and several others. The `ScopeLink` only deals
with variable binding in a general setting. Th `LambdaLink` is reserved
for functions. For example, the expression "`forall x, P(x)`" binds the
variable `x`, but it is not a function, and thus not a lambda. We
conclude that `ForAllLink` would need to inherit from `ScopeLink`, and
not `LambdaLink`, even though they are syntactically identical.

The dis-assembly/re-assembly of the `ImplicationLink`s into parts is
managed by `RewriteLink`; it inherits from (builds on) `ScopeLink`; the
vardecls are managed by `Scope`, the rewriting by `Rewrite`.

Some rewrites are not of the direct form shown above, but can leave
vardecls stranded inside of function bodies. This is fixed with the
`PrenexLink`, which inherits from `RewriteLink`, and provides some
additional code to move variables out, and create a final expression
that is in prenex form. This typically is triggered by inference rules
for function composition. For example, suppose we have functions
(lambdas) `f(x,y)` and `g(z,w)` and we wish to create `f(x,g(z,w))`
This requires moving the embedded vars `z,w` to the left, so that the
final form is a prenex `(fg)(x,z,w)`. The `PrenexLink` deals with this.

To summarize: `RuleLink` inherits from `PrenexLink`. `Prenex` inherits
from `RewriteLink`; this inherits from `ScopeLink`.

### Executable conclusions
One more example is in order. Consider the inference rule for function
composition.  In Gentzen notation, this is:
```
    Q(x) ,  x=A(y)
    --------------
       Q(A(y))
```
This inference rule is represented in Atomese as follows:
```
	(RuleLink
		(VariableList
			(Variable "$vardecl-x") (Variable "$Q")
			(Variable "$vardecl-y") (Variable "$A"))
		(And
			(LocalQuote
				(Lambda
					(Variable "$vardecl-x")
					(Variable "$Q")))
			(LocalQuote
				(Lambda
					(Variable "$vardecl-y")
					(Variable "$A"))))
		(LocalQuote
			(Lambda
				(Variable "$vardecl-y")
				(PutLink
					(Variable "$vardecl-x")
					(Variable "$Q")
					(Variable "$A")))))
```
As before, the `AndLink` says that there are two premises, to be
combined. The `LocalQuoteLink` says that each each `Lambda` is a
literal, and not to be interpreted, run or executed.

The `PutLink` is interesting: it is used to perform the actual
beta-reduction required by function composition. Whatever it was that
was `A` is substituted for `x` in the body of `Q`.  The `PutLink` is
*not* quoted: it is assembled (by the `RuleLink`) and then executed
(so that it plugs `A` into `Q`.)

Note that there are two distinct beta reductions in the above. The
first plugs in for the variables "$vardecl-x", "$Q", "$A" to assemble
the `PutLink`, and the second one is done by executing the `PutLink`
itself.

This is meant to illustrate that conclusions can be executable, in
general. For example, for arithmetic, the inference rules might be for
addition, subtraction, multiplication, division. The role of `PutLink`
above is then taken by `PlusLink`, `MinusLink`, `TimesLink`,
`DivideLink`. These are not just declarative elements, but are
executable; executing `(Plus (Plus x 2) (Plus y 3))` results in the
delta-reduction of the constants: `(Plus x y 5)`

### Algebra as an axiomatic system
The above presents one more recursive puzzle. How did we know that
`(Plus (Plus x 2) (Plus y 3))` can be rewritten into `(Plus x y 5)`?
Ah, well... five answers.

The first answer is that the c++ class `PlusLink` actually implements
c++ code to actually do this. It works, at least for simple expressions.

The second answer is that, instead, we "should have" written some
inference rules that capture the rules of delta-reduction on arithmetic
expressions.

The third answer was to write down the Peano arithmetic axioms in
Atomese. This is an interesting theoretical distraction, but becomes a
non-starter, if one also wants the reduction to be high-performance.

The fourth answer is that "we could have" selected some Computer Algebra
System (CAS) and wrapped it up in Atomese, so that whenever we called
the c++ method `PlusLink::reduct()`, it would call into this CAS system
to do its magic. These arguments promptly go sideways. One person says
"oh use Maxima", and the next person makes a bold leap and says "use HOL
or use Agda", the problem here being that HOL and Agda are not CAS
systems, but are rather proof theoretic inference engines. The
head-scratcher then turns into a question of how to discard the URE and
plug Agda into it's place. This was discussed in an idle fashion, but
not acted upon. A different suggestions that circulated was to use to
turn the URE into a wrapper around the Uni Potsdam ASP solver.

The fifth answer, the one I'm currently pursing, is to create
sensorimotor agents capable of perceiving algebraic structures (seeing
or sensing them), and then acting on them (the motor or manipulative
part, which grabs a hold of axioms and rules, and manipulates them in a
purposeful way.) Its an agent, because it distinguishes it's own self
from the external world. The agent holds a model of the external world,
representing what is "out there", and then manipulates the external
world in conformance to inferences obtained by acting on it's world
model.

The current test case for this fifth answer is the `atomese-simd`
project. Here, the "external world" is a GPU, and the design goal for
the agent is to be able to take algebraic expressions like `(Plus x 2)`
and have them run on the GPU.

Don't be mislead: of course, one can just hard-code some code that does
arithmetic on GPU's. That's easy. You can do that. You can ask Claude or
ChatGPT to do it for you. They'll do it. The issue here is that you
would then have to do this again for each new axiomatic system. First,
for all kinds of different areas in mathematics. Next, for accounting
and purchase order requisitions. Finally for factory blue prints and
executive org charts. Yes, currently humans are asking LLM's to design
each of these, but the designs are always human-guided. The goal of
designing an axiomatic shape-rotator is to have the shape rotator
perceive and manipulate any kind of axiomatic system, and not just the
one for arithmetic ported to GPU's.

But I digress...

### GuardLink redux
The review above indicates the implementation for the `GuardLink`,
and where it needs to be in the inheritance hierarchy.  It needs to
inherit from `ScopeLink`, and `RewriteLink` needs to inherit from
`GuardLink`.

Neither the `Rewrite` nor the `Prenex` are executable. Both provide
various different beta reduction methods. The method
`PrenexLink::beta_reduce()` takes a `HandleMap` holding a grounding,
and performs beta reduction with the provided grounding.

The `GuardLink` needs to provide a `guard()` method that takes either
a `HandleMap` or `HandleSeq` as an argument, and returns a bool
true/false, indicating if the guard will allow this grounding to pass.

`GuardLink` probably needs to be executable, in analogy to `RuleLink`,
so as to reduce it's own body. Viz move that code out of `RuleLink`.

get_variables().substitute(body, vm, _silent);
Variables::substitute throws when guard fails.
Replacement::substitute_scoped
Variables::is_type() vs atoms/signature/TypeUtils.cc type_match()
move FilterLink::extract to GuardLink!?!!

Done. GuardLink has been implemented.


### Guarded connections
There is also another, distinct idea of guarding the connectionist
rules. So, in the basic sheaf-theoretic conception of establishing
connections, the connectors are examined, and the mating rules are
compared, generating a go/no-go answer to the possibility of mating.
A guard clause would be an additional evaluatable predict that is
invoked at mating time, performing some additional algorithmic check,
allowing or disallowing the connection to go forward.

How would this work? Where would these guard terms be stored?


### TODO
* Fix Connectors so that they can be named and typed as expected.
  (Fix?? Where? What?)
* Should Connectors be Unordered??
* Should we formalize the idea of a BoolLambda?
