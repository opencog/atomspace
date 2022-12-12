Objective Atomese
=================
An object-oriented infrastructure, in Atomese.  Version 0.0.0; there is
nothing here yet, except for a sketch of ideas.

Motivation
----------
The primary goal is to replicate some fraction of the code in the
[matrix](../matrix) directory, but to do so in pure
[Atomese](https://wiki.opencog.org/w/Atomese), instead of scheme. The
matrix code has proven to be exceptionally useful and powerful, but there
is now a need to be able to run it in (parallel) pipelines (configured
with [ProxyNode](https://wiki.opencog.org/w/ProxyNode)s.) This includes
running it over the network (via
[CogStorageNode](https://wiki.opencog.org/w/CogStorageNode)) or otherwise
interacting with [StorageNode](https://wiki.opencog.org/w/StorageNode)s
in interesting, non-trivial ways. For that, the matrix code must become
specifiable in Atomese. (Yes, we could just wrap it in a
[GroundedProcedureNode](https://wiki.opencog.org/w/GroundedProcedureNode)
but that would be ugly and counter-productive.)

Object Basics
=============
The actual code in matrix is written in scheme, and uses a peculiar OO
programming style variously called
[Generic Programming](https://en.wikipedia.org/wiki/Generic_programming),
[Parametric Polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism)
or [Traits](https://en.wikipedia.org/wiki/Trait_(computer_programming)).
In short, it is an OO style similar to that of JavaScript, rather than of
C++.


How might this work in Atomese?  Maybe like this:
```
(DefineLink
	(DefinedMethodLink
		(PredicateNode "some object instance")
		(PredicateNode "some method"))
	(LambdaLink
		(VariableList ...)
		... atomese ...))
```
The `DefinedMethodLink` isn't really needed; one could just use a
`ListLink` and this would work just fine, today. But it's convenient to
have a special link type devoted to this task.

Invocation of the object-instance+method paure is "just like always", with
the [ExecutionOutputLink](https://wiki.opencog.org/w/ExecutionOutputLink):
```
(cog-execute!
	(ExecutionOutputLink
		(DefinedMethodLink
			(PredicateNode "some object instance")
			(PredicateNode "some method"))
		(ListLink
			... method args ...)))
```


