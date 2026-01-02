Design Notes B -- Assembly Theory
================================
Start all over again.

So an old idea is that the assembly of parts should work like jigsaw
pieces.  Each jigsaw connector should have some type description. This
is some mashup of type theory, ideas from Link Grammar, and ideas like
introspection from Java or from D-Bus.  That, at least, is the sexy
idea. The actual practice is anything but -- just a lot of carefully
designed, hand-built pipelines. I got enough of the idea across that
Claude could build some of them, but I had to supervise.  There's no
general assembly mechanism, and this remains far off and unworkable
despite repeated attempts.  What's going wrong?

Two or three things seem to be desirable:
* Something other than the use of anchors as attachment points where
  SetValue and ValueOf can rendezvous.
* Pipelines need to be descriptively defined.
* It would be OK if there was some constructor that performed the actual
  wiring, given the description. That is, the description itself does
  not have to be runnable; it just needs to be compilable/assemblable
  into something that runs ...

There are two meta-goals I have to decouple:
* The short-medium term issue of making pipelines defacto easier to
  write, over the coming months. Ideally using some descriptive
  framework having some OK properties.
* The long-term goal of self-assembly and recursive algorithmic
  self-design.

Pipeline Assembly
-----------------
So what happened when I wrote `(FlatStream (SortedValue))`?
* I designed a c++ class called `FlatStream`, whose ctor takes a
  single Value. It pulls from that Value, by driving it's `update()`
  method.
* There is (currently) no Atomese description of this API, although
  both `FlatStream` and `SortedValue` have SIG's and ARG's that allow
  them to be attached via the class factory.
* These SIGs, ARGs and the general type hierarchy is NOT stored in the
  AtomSpace.
* Despite things like TypeInhNode, etc. there is no actual way that
  an Atomese expression can be written to walk/explore/analyze the
  type hierarchy.
* There is no definition, either hand-written or auto-generated, that
  expresses (re-expresses) the SIG/ARG type constructors/constraints
  in terms of `Connector`, `ConnectorSeq`, `Section` that we've been
  giving lip-service to.
* There is no way to take a description written in terms of Connectors
  and converting it into an actual assembly like
  `(FlatStream (SortedValue))`

FWIW, There's the unexplored alternative of hooking these up with
the existing anchor-point design.
```
(cog-execute! (SetValue (Anchor "foo") (Predicate "key")
	(LinkSignature (Type 'SortedValue) ...?))

(cog-execute!
	(LinkSignature (Type 'FlatStream) ...
		(ValueOf (Anchor "foo") (Predicate "key"))))
```

* There's no way to convert the anchor point design into a
  hierarchical design, or back.
* Any given anchor-point connection is not validated via the SIG
  mechanism. Invalid anchor-point connections are easily constructed.

Wow. Those are like six really damning design flaws. That I have not
had to face yet. How did that happen?

What's the priority order for fixing any of this?

Connectors and Pipes
--------------------
To arrive at a connector-based syntax for hooking up pipelines, lets
start with a simpler, more "obvious" solution. Consider having an
`InputNode`. It can be used anywhere a stream value is used, and acts
as a place-holder or "promise" for future input. Thus, one can write
```
   (FlatStream (InputNode "input-for-flattener"))
```
Polling `FlatStream` would call `InputNode::update()` which then blocks,
as no actual source of input is connected.

The directionality of `Input` and `Output` labels is mildly confusing,
while `ConsumerNode` and `ProducerNode` are perhaps less so?

To provide an input, one writes:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ProducerNode "output-from-sorter"))
```
Creating this PipeLink and placing it in the AtomSpace would wake up
the `ConsumerNode` (via c++ `std::condwait` on some `std::mutex`) and
then (magic happens here) it gets stream data from `OutputNode`.

How might that work? Well, lets try this:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ValueOfLink (Anchor "some place") (Predicate "some key")))
```
which should "work as expected", except that we're once again using
Anchors, which is a design point I kind of want to move away from.

Side comment: The `ValueOf` returns `VoidValue`, unless a `SetValue`
was done earlier. The `VoidValue` is already used as an end-of-stream
marker, so the consumer would immediately receive an end-of-stream
unless the producer had been previously declared. There's no obvious
way  of fixing this without inventing some `WaitVauleOf` that camps
on that key, waiting for something to be attached. This does need to
be fixed; we do want to be able to define flows in arbitrary order,
so that they can e.g. be retrieved from Storage.

Lets try some others:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(MeetLink ... pattern...)
```
Clearly this wires up the output of the `Meet` to the consumer. No

Another:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ExecutionOutput ... )
```
This wires up the output of the `ExecutionOutput`. No problems here.

The generic form is then
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		( ... producers ... )
```
The `( ... producers ... )` are any c++ class `FooLink` with an
`FooLink::execute()` method on it.

The output of the execution is not routed "anywhere", it just arrives
"here", an unspecified, anonymous "here and now" when `FooLink::execute()`
is called.

There is no obvious design for a `OutputNode` aka `ProducerNode`.
So the initial motivating example above is flawed.

### Here and Now
Earlier designs used `Lambda`s:
```
    (Define (DefinedProcedureNode "named function")
       (Lambda
           (VariableList ... inputs ...)
           (... body ...)))
```
Here, the input variables are explicit placeholders; but they only
provide half of what `ConsumerNode` provides: we know what the inputs
are, but we don't know where they "came from".

The `ExecutionOutput` provide "half" of `PipeLink`:
```
   (ExecutionOutput
      (DefinedProcedure "named function ")
      (... producers ...))
```
The output of the execution is not routed "anywhere", it just arrives
at the unspecified, anonymous location of "here and now".

The `CollectionOf` provides a way of wiring single-output streams into
place:
```
   (CollectionOfLink
      (Type 'SortedValue)
      (... producers ..))
```
Written in this way, it suggests that `CollectionOfLink` and
`ExecutionOutputLink` can be collapsed into one common do-it-all Link.

### Types
However, there is a big difference: `CollectionOf` was meant to be a
type-casting device: Whatever type the producers are producing, the output
will be recast to the indicated type.

This is in contrast to the `LambdaLink`, which uses `TypedVariable` to
specify the input types.  But what, exactly, is the point of that? If
the input types don't match, then what? Ignore them? So, an implicit
`FilterLink` behavior? Or throw an exception? So, while `TypedVariable`
makes sense for pattern matching, it is not at all clear why or how
Lambdas should respect it.

There is no Atomese way to work with those type declarations, either,
except to the extent provided by the current `RuleLink`. Lambdas do
not specify their output type either: and if they did, what would happen
if the Lambda body failed to generate the specified output type? Ugh.

There's yet more confusion. Typed variables are explicitly named: they
are not anonymous. But those names do not matter to external observers;
the names only matter to the internal body. Externally, the variables
may as well be anonymous; only their positional value matters. This is
the mess that leads to the need for alpha-conversion, when there are
naming conflicts. Ugh.

The original conception for `SignatureLink` was to allow *anonymous*
type signatures to be written down. Although it is possible to compute
a `SignatureLink` from a given `Lambda`, there is currently no code
that actually does this.  This is not hard to solve:
```
    (SignatureOfLink (Lambda ...))
```
when executed, would create a `SignatureLink` that corresponds to the
Variable declarations in the Lambda. There already is a `VardeclOfLink`
that returns the vars in raw form.

### Signature
Lets take a short moment to give an explicit example. Consider
```
   (SignatureOfLink
       (VariableList
            (TypedVariable (Variable "foo") (Type 'Concept))
            (TypedVariable (Variable "bar") (Type 'Predicate))))
```
when executed, would return:
```
    (SignatureLink
       (ListLink
          (Type 'Concept)
          (Type 'Predicate)))
```
That's it -- The list preserves the positional value of the types, while
discarding the associated names.

### Output Types
The function type constructor is `ArrowLink`, indicating input types on
one side, output types on the other. It was to correspond to the function
type in ML, CaML, Haskell, etc.

There were plans to create `CoArrowLink`, the category-theoretic "opposite"
of the arrow. This was never done.

There were plans to create `LeftArrow` and `RightArrow`, corresponding to
associative monoidal left and right multiplication. This was never done.

There were vague ideas of talking about arrows as functors. This would
have then required left and right adjoints.

Natural transformations could then be interpreted as homotopic
refactorings of Atomese, i.e. transformations between two different
Atomese representations for the "same thing".

There was talk of applying such natural transformations automatically,
as a convenience to the user of Atomese as a KR system.

### Shape rotators
This very rapidly slides down the slide of re-interpreting graph
rewrites as theorems, or at least rules, and then forward/backward
chaining them. Or, alternately, using ASP as a constraint solver.
Which then calls for a generalized axiomatic system.

This is, of course, what a shape rotator is. Insofar as LLM's are
wordcels, the core project of Atomese, and the AtomSpace, is to design
a generic shape rotator that can perceive and work with and manipulate
shapes. To work natively with axiomatic systems, proof-nets, sheaves
of interconnecting jigsaws. So, here we are. That's why we do this.

### Connectors and ConnectorSeqs
An early idea was to use `ConnectorLink` to specify inputs and outputs,
with `ConnectorSeq` bracketing a collection of connectors into a jigsaw.
The jigsaw would simultaneously generalize the notion `SignatureLink` and
`ArrowLink`.  What was never created was a formal specification for moving
between those types, and jigsaws. Lets try this now:

```
   (JigsawOfLink
       (VariableList
            (TypedVariable (Variable "foo") (Type 'Concept))
            (TypedVariable (Variable "bar") (Type 'Predicate))))
```
when executed, would return:
```
    (ConnectorSeq
       (Connector
          (Type 'Concept)
          (SexNode "input"))
       (Connector
          (Type 'Predicate)
          (SexNode "input")))
```
which indicates both the type, and the directionality. (This might not
be quite right; further below, there's a proposal to include the names.

### Named Outputs
The proposal above, repeated here, can now be critiqued and deconstructed:
```
   (PipeLink
		(ConsumerNode "input-for-flattener")
		(ProducerNode "output-from-sorter"))
```
The difficulty/impossibility of defining the `ProducerNode` indicates
that it does not exist. Then naming ambiguity of Input/OutputLink
indicates that it is neither, or both. The examples all were of the form
```
   (PipeLink
		(NameNode "named output")
		( ... producer ...))
```
Here, the producer, producing anonymous, unnamed values via calls to
it's `Producer::execute()` method, has a name assigned to that output.

The consumer takes several forms:
```
   (FlatStream (NameNode "input-for-flattener"))
```
Oh no ...! Should that be `(NameNode "named output")`? Is it an input
or an output? Well, its both... the naming difficulty persists.

Anyway, a `FlatStream` is not an Atom, so the correct representation is
this:
```
   (CollectionOfLink
      (Type 'FlatStream)
      (NameNode "input-for-flattener"))
```
The `NameNode` is now seen to be dual to `VariableNode`. Variables are
used internally in a Lambda, to ease the wiring up of the guts inside
the Lambda. The outside does not care about this wiring, or these names.
The `NameNode` is the opposite: only the outsides care: the wiring
diagram is extra-mural.

Huh. So the above paints the Lambdas as a cellular wall, dividing inside
from outside. Curious. I've never quite thought of it that way.

### Directionality
The directionality is implicit. In `PipeLink`, the `NameNode` comes
first; thus it is the sink for whatever follows. In `CollectionOf`,
it is second, therefore the source. This inputs-come-second pattern
is generic in Atomese:
```
    (ExecutionOutput
       (function name or definition)
       (arguments))
```
or
```
    (FilterLink
       (filter definition)
       (items to be filtered))
```
For minimalist, human-authored Atomese, this is sufficient. For a
connectionist approach, we want more. For example, executing
```
    (JigsawOfLink
       (ExecutionOutput
          (function name or definition)
          (arguments)))
```
should return
```
    (Section
       (function name or definition) ; this is the "name" of the section
       (ConnectorSeq
          (Connector
              (TypeNode 'Value)   ; type of output; generic Value
              (SexNode "output")) ; its an output
          (Connector
              (TypeNode 'Foo)     ; type of first argument
              (SexNode "input"))  ; its an input
          (Connector
              (TypeNode 'Atom)    ; type of second argument
              (SexNode "input"))))  ; its an input
```
Here, the output type of the `ExecutionOutput` is known a-priori; it is
always a `Value` because the c++ method is `ValuePtr Atom::execute()`.

Even when the output is named:
```
   (PipeLink
		(NameNode "named output")
		( ... producer ...))
```
it has to be assumed to be a generic `Value`. We can narrow the type:
```
   (PipeLink
      (Connector
         (TypeNode 'FooNode)
         (SexNode "output"))
		( ... producer ...))
```
The above types the output, but the output remains anonymous. To name
it, we have to get more verbose:
```
   (PipeLink
      (Section
		   (NameNode "named output")
         (Connector
            (TypeNode 'FooNode)
            (SexNode "output")))
		( ... producer ...))
```
This is verbose and unpleasant to hand-write. Thus, for hand-written
pipelines, the un-typed but named `NameNode` seems to be the way to go.

### Alternative styles
Are there other ways?  Well, historically we have
```
   (TypedVariable
      (Variable "var name")
      (TypeNode 'Foo))
```
We concluded earlier that `SignatureLink` always strips off names.
Therefore,
```
   (SignatureOfLink
      (TypedVariable
         (Variable "var name")
         (TypeNode 'Foo)))
```
returns
```
   (TypeNode 'Foo)
```
while
```
   (JigsawOfLink
      (TypedVariable
         (Variable "var name")
         (TypeNode 'Foo)))
```
returns
```
   (Section
      (Variable "var name")
      (Connector
         (TypeNode 'Foo)
         (SexNode "input")))
```
which indicates that it is always the case that such typing expressions
are talking about inputs.

Unlike earlier proposals, the `JigsawOf` here is attempting to propagate
the name. This is not obviously correct.

Earlier, it was suggested that
```
   (JigsawOfLink
       (VariableList
            (TypedVariable (Variable "foo") (Type 'Concept))
            (TypedVariable (Variable "bar") (Type 'Predicate))))
```
when executed, would return:
```
    (ConnectorSeq
       (Connector
          (Type 'Concept)
          (SexNode "input"))
       (Connector
          (Type 'Predicate)
          (SexNode "input")))
```
Now we seem to be suggesting that, instead, it should return named
connectors:
```
    (ConnectorSeq
       (Section
          (Variable "foo")
          (Connector
             (Type 'Concept)
             (SexNode "input")))
       (Section
          (Variable "bar")
          (Connector
             (Type 'Predicate)
             (SexNode "input"))))
```
This is verbose, and awkward-seeming. Perhaps
```
    (ConnectorSeq
       (Connector
          (Variable "foo")
          (Type 'Concept)
          (SexNode "input"))
       (Connector
          (Variable "bar")
          (Type 'Predicate)
          (SexNode "input")))
```
### Homotopy engines
Two remarks:
* None of this blabber about sections and signature really matters until
  we have some assembly engine in place.  The above are examples of the
  type specifications that the assembly engine would need to work with,
  but only experimental experience can hone the details.
* The malleability and flexibility (and thus, uncertainty) of the
  representation is a central feature of such axiomatic systems. That is,
  there are homotopic representations of any given structure, and there
  are homotopic deformations that preserve the semantics. From the
  distance, it seems that many different kinds of representations are
  adequate. Any one can do.
* We do not have a homotopy engine: a system for transforming one kind
  of type specification into another, equivalent one.

The idea of a homotopy engine sends me into a recursive tail-spin.
Homotopies are necessarily definable as a collection of rewrite rules,
each rewrite being a valid homotopic transformation in its own right.
The homotopy engine is then nothing more but a rule engine that applies
transformations from the homotopy rule set.

As to rule engines: been there; done that.  At least part of the idea
of ML, CaML, Haskell is that the rule engines for types are simpler
than of the code itself. (I view a compiler as a big complicated,
special-purpose rule engine.)

Does typing become recursively simpler? That is, the rules for a type
engine are few in number and simple in structure: function composition,
something more for relations.  The rules themselves have "types":
functions can be plugged into relations, but not vice-versa. Relations
are always (bool) true/false.  The rule system for the rule system
is going to be even simpler? What's the "final ultimate grounding",
is it set theory? Category theory? Something else? Something simpler?
This is hard to talk about without explicit examples. But it again
highlights that an appropriate rule engine remains out of reach for
Atomese.

### Assembly engines
That is, we need an assembly engine capable of assembling jigsaws, and
we need an exploration of th expressive power of jigsaws: we need to
convert any kind of axiomatic system, the axioms and inference rules
for "anything at all" into jigsaws. I'm still floundering here.

This seems like step one for building a shape rotator. I can watch
Claude "think" via wordcel constructions; its clear that wordcel
representations of shapes are possible, but quite inefficient. But I
digress...

The conclusion seems to be:
* The discussion of Signatures, Types, Connectors and Sections
  constitute a small step towards a rule engine, (from which a homotopy
  engine or an axiomatic theorem-proving engine could be built) but it
  remains out of reach, and of seemingly low priority, and won't be
  explored further(?) in this text(?)

### Duality
The `PipeLink` and `NameNode` appear to be dual, in some sense, to
`DefineLink` and `DefinedProcedureNode`. This is worth (another)
quick review.

Consider the (valid) expression, given earlier:
```
    (Define
       (DefinedProcedureNode "named function")
       (Lambda
           (VariableList ... inputs ...)
           (... body ...)))
```
This takes an anonymous, un-named `Lambda` and gives it a name.  What,
exactly, is given a name? The output, or the function? It has to be the
latter, since the `Lambda` by itself "does nothing" unless it is applied
to some arguments (with the `ExOutLink`.)

It is tempting to write the (invalid, incorrect) expression:
```
    (PipeLink
       (NameNode "named function")
       (Lambda
           (VariableList ... inputs ...)
           (... body ...)))
```
Why is this invalid? The intent of the `NameNode` is to name the output,
not the function. The `Lambda` body is utterly incapable of providing
any output: it would need to be applied to some inputs.  That is, the
`NameNode` is to be applied, as a label, to an already-available output,
or a future promise of output. The mechanics at the "other end" are of no
concern.

Thus, the duality: `DefinedProcedure` names functions requiring inputs,
and provides a convenient handle with which the function can be applied
to those inputs.  Or, using a more connectionist vocabulary: it provides
a convenient name for attaching input streams to the input side of a
processing function.

The `NameNode` gives a name to the output side. There's no thought of
what the inputs may be: they are irrelevant to the output. All we know
is that there is some output stream of data, and, in order to avoid it's
anonymity, we want to give it a name.  That name is then a stand-in for
a data source: anything that has a name is guaranteed to deliver data.

Thus, for example, we have a valid use of `NameNode`
```
   (CollectionOfLink
      (Type 'FlatStream)
      (NameNode "input-for-flattener"))
```
The intent is clear: the `NameNode` will deliver data, and the flattener
will flatten it.

By contrast, the expression below is invalid/wrong:
```
   (CollectionOfLink
      (Type 'FlatStream)
      (DefinedProcedureNode "input-for-flattener"))
```
It's non-sensical, more or less: The `DefinedProcedure` needs inputs;
it can't provide anything to the flattener.  I wrote "more or less"
only because one can imagine, for this example, that the lambda
arguments can be hoisted, and this expression put into prenex form,
so that it is interpreted as a brand-new (un-named) lambda that is a
function composition of the anonymous flattener, and whatever came
before.

### Composition
Many years ago, there had been proposals for a `ComposeLink`; these were
nixed, because the existing infrastructure for `ExOutLink` could already
do function composition (the `PrenexLink` was implemented: it hoists the
variable declarations to the front.)

Function composition can be thought of as a certain axiom that describes
how functions are to be combined.  That is, if functions are the objects
of your theory, then the application of one function to another can be
thought of as an associative operation: that thing that you get when you
take the function symbols `f` and `g` and write them next to each other:
`fg`. This is a basic axiom of category theory. In the language of lambda
calculus, this winds down the roads of application, beta reduction,
combinators.  It is more-or-less "fully" implemented in Atomese by the
`PrenexLink`.

But this is all a red herring; irrelevant to the current task: the goal
here is to process data streams, and to specify the wiring diagram that
performs that processing.  Lambdas and function composition provide the
wrong abstraction, or perhaps an inconvenient abstraction:  yes, of
course the expression `fg` means "connect `f` to `g`, but this is
unmanageable, if we were to specify an electrical circuit in this way.
(Of course, Verilog and VHDL are not the answer, either.) The point of
the connectionist approach is to say "this is attached to that" and to
find a simple way of writing that down.

The `PipeLink` and `NameNode` are meant to make pipeline authoring easy.

### Pipeline authoring recap and conclusions
A quick peek at how `PipeLink` and `NameNode` replace and improve on
the previous idea.

Before:
```
   (FilterLink
      (RuleLink ...)
      (ValueOf (Anchor "anchor name") (Predicate "name of stage")))
```

After:DEFINED_PROCEDURE
```
   (FilterLink
      (RuleLink ...)
      (NameNode "name of stage"))
```
So clearly, its more compact. The c++ implementation of a `NameNode`
class also allows for customized handling of promises, streams,
blocking, etc. The precise design of which is TBD. (Note the earlier
confusion about blocking and streaming; it persists here.)

Before:
```
   (SetValue
      (Anchor "anchor name") (Predicate "name of stage")
      (... some stream expressed in Atomese ...)

```
After;
```
   (PipeLink
      (NameNode "name of stage")
      (... some stream expressed in Atomese ...)

```
When is was difficult to express the initial stream in pure Atomese, the
scheme `(cog-set-value! atom key value)` function was used: but this is
an end-run, a fallback to scheme, instead of pure Atomese. We will avoid
creating a `(cog-set-pipe-source! name value)` function to provide a
similar end-run.

### VHDL and Verilog
Contrast and compare. In VHDL, one would write:
```
   architecture ... of:
       signal func_result : std_logic_vector;
       signal overflow : std_logic;
   begin
       U1: entity func
          port map (
             output => func_result,
             ...
          );
       U2: entity register
          port map (
             input => func_result,
             ...
          );
   end architecture
```
The difference between this and Verilog syntax is minor.

So `(NameNode "func_result")1 becomes `signal func_result` there.  The
`NameNode` is untyped, by default; a type declaration would be
```
   (Section
      (NameNode "func_result")
      (Signature
          (TypeNode 'BoolVec)      ; The logic signal
          (TypeNode 'BoolValue)))  ; The overflow/error "signal"
```
The second part of  the signal wire is not given a distinct name here;
it could have been, but isn't. Instead, its positional.  The `Section`
uses a `Signature` here, instead of a `ConnectorSeq`, because both ends
of the wire are "the same": wires have no polarity, and don't transform
what they carry. The use of `Signature` replaces the need for declaring
two connectors, one at each end of the wire, having exactly the same
type, differeing only in Sex.

The type declaration for the wire (the `Section`) can appear anywhere,
and does not have to be grouped with the `Name`.  Likewise, the usage
of `NameNode` does not have to be grouped, either; whatever it connects
to, it connects to.  No surprise: Atomese is not a text file format.

The terms "signal" or "wire" or "port" or "portmap" do not seem to offer
any improvement over `NameNode` and `PipeLink`.


The End
=======
Continued in [Design Notes C](Design-Notes-C.md).
