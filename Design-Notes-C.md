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


