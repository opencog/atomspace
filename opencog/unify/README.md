Unifier
=======

The unifier is a component to perform
(unification)[https://en.wikipedia.org/wiki/Unification_(computer_science)]
between atoms. It is an essential component of the Unified Rule
Engine, URE for short, as it allows to check if a given source
(resp. target) matches a given premise (resp. conclusion). The reason
pattern matching is not enough is because in the URE both sources
(resp. targets) and premises (resp. conclusions) may contain
variables, while in the pattern matcher only the patterns contain
variables, not the groundings (or if they do these are interpreted as
constants).

At its core what the unifier does is finding assignements to each
variables in the terms that would render them equal by substitution.

For instance given the following 2 terms

```scheme
(Inheritance
  (Concept "A")
  (Variable "$Y"))
```

and

```scheme
(Inheritance
  (Variable "$X")
  (Concept "B"))
```

the unifier would return the following assignment as a solution:

```
{ (Variable "$X") -> (Concept "A"),
  (Variable "$Y") -> (Concept "B") }
```

meaning that if `$X` is subtituted by `A` and `$Y` is substituted by
`B`, the 2 terms are equal.

Sometimes assignments may map variables to other variables, or to
subhypergraphs containing variables, etc, but the ideas remain the
same.

Status
------

The unifier is virtually complete and can handle most situations,
including cyclic dependencies and typed variables. It is reasonably
efficient, though, due to the inherent cost of unification, could
welcome some optimizations. Beyond that its main limit is the lack of
deep type support.

Usage
-----

For the moment only a C++ API is available. The entry point is the
functor class `Unify` that is contructed with 2 terms as follows

```c++
Unify unify(t1, t2, t1_vardecl, t2_vardecl)
```

where `t1` and `t2` are Handles, and `t1_vardecl` and `t2_vardecl` are
optional variable declarations to specify their types.

Then it can be called via its `operator()`

```c++
Unify::SolutionSet ss = unify();
```

where `Unify::SolutionSet` is a class representing a mapping between
variables appearing in both terms and their most abstract values
(values in a broad sense as such may be variables, or subhypegraphs
containing themselves variables).

The `Unify` class contains also convenient methods to carry out
substitutions on the terms and given their unification solutions,
which is heavily used by the URE.

Examples can be found in `tests/unify/UnifyUTest.cxxtest` containing
dozens of tests, ranging from very simple to quite complex.

TODO
----

* Support deep types
* Support GlobNode
* Add Scheme bindings
* Implement memoization at the subcall level. Indeed the unifier is
  stateless, and is writen in a functional way relying on recursivity,
  thus is likely an excellent candidate for subcall memoization.
  Queries sharing subhypergraphs, which is common in the URE context,
  would likely benefit from such optimization.

Author
------

Nil Geisweiller
