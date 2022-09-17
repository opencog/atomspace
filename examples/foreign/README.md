
Foreign Abstract Syntax Tree (AST) Examples
===========================================
Consider the idea of a database that stores JSON expressions. JSON is a
way of representing data in a certain labelled key-value format. JSON
can be thought of as a certain kind of abstract syntax tree (AST) for
data.  The AtomSpace is a knowledgebase that stores trees. Therefore,
one ought to be able to store JSON in the AtomSpace.

Such databases already exist, and some are even popular.  This is
because JSON is a fairly reasonable way of representing structured data.
The goal of these examples is to observe that the source code for almost
every programming language can be decomposed into an abstract syntax tree.
With a parser in hand for some specific language, the resulting trees can
be stored in the AtomSpace. That means that the language in question can
now be used as a knowledge representation system, it can be used to
store data, in the conventional sense of "a database".  That database
comes with a fairly powerful search engine/query system, "for free" --
its provided by the AtomSpace pattern engine.

To the best of my knowledge, there does not exist any database that
allows you to store data in a python format. If there was such a thing,
you could store python in it. For example one could store the python3
code
```
  x = list(("apple", set(( 1, 2, 3)), "cherry"))
  print("the result:", a+b)
```

Note that, because it is being stored, and not executed, the `a` and `b`
in the example above do not need to be defined. A query on this dataset
might be "find all expressions containing the word 'cherry'" or "find
all expressions containing the symbol 'print'".  Careful, though: the
goal here is not to just store "source code"; that would be silly. Every
competent programmer has thier favorite tools for searching source code
for certain expressions. The point here is that vast oceans of trees can
be stored, and very complex queries can be performed on the data.
Explcitly, the data is represented as a graph, and the queries are
written in a (hyper-)graph query language (HQL).

Well, that's all very nice. But for now, the existing code is still
experimental and very incomplete.  The demos explore the limits of what
is actually possible.

* `sexpr-query.scm`         -- Working with s-expressions.
* `metta-lisp.scm`          -- MeTTa emulation example.

TODO
----
The sexpr code is a proof-of-concept. A slightly more usable API could
be something similar to or even fully compatible with "OGRE", the
[Open Generic Representation](http://binaryanalysisplatform.github.io/bap/api/odoc/ogre/Ogre/index.html)
We already have all the needed bits and parts (DefineLink, ArrowLink,
SignatureLink, etc.) it just needs to have a pretty user API wrapped
around it.
