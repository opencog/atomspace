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
does everything for everyone? Or is it easier to keep them distinct?

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

I suspect that the above three do not prsent any serious difficulties.
However, I think there is a huge stumbling block that does need to be
dealt with, already started in Design-Notes-A: this is the issue of
streams vs. lists vs. sets, represented as Atoms or as Values, streaming,
or not, blocking, or not.   Applying functions is easy. Untangling the
intended semantics of different kinds of data streams is hard.

### Comments
Maybe some of the above is wrong.
* The `GroundedProcedureNode` is "by definition" without any type
  declarations.  The `ExecutionOutputLink` has no issue with this:
  it just applies whatever arguments are given. The FilterLink could
  do this too, but its less interesting, less compelling.
