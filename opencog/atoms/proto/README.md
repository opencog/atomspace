
ProtoAtoms
==========
ProtoAtoms are a partially-implemented, incompletely thought-out
concept for a way of associating arbitrary generic values with some
given atom.  ProtoAtoms are like atoms, except that they lack a TV,
an AV, and cannot be stored in the atomspace.  This makes them smaller,
lighter and more efficient.

However, because thier design is incomplete, the best way to use them
is not yet clear.  The current status is tracked in
[github bug #513](https://github.com/opencog/atomspace/issues/513)
The current best ideas for usage, and some remaining questions,
are below.

Design concerns
---------------
If protoatoms don't behave (exactly) like atoms, then  they need yet
another, new API to access them. More API's mean more complex code,
tricker algorithms, more coding styles, some of which are squirmy,
because the programmer never quite bothered to think it through,
(because they were too lazy to figure out what protoatoms were or how
to use them effectively).

An example of a failed design point is to store them as association
lists, as a part of the atom: [implemented
here](https://github.com/opencog/atomspace/pull/797/files)
This was a bad idea precisely because it added complexity and changed
the API in undesirable ways.

So, the goal/trick is to find some way of using them that fits
naturally.  Not quite sure what that is.

EvaluationLink-like
-------------------
Here's one idea:

```
   ProtoEvaluationLink
         PredicateNode "name of the attribute, e.g. AV"
         SomeAtom
         FloatValue   # which is a vector of floats holding the actual av
```
 * Where SomeAtom is the atom to be tagged with the attribute
 * Where the PredicateNode is an actual node, so that it's in the
   atomspace, its globally unique, and thus easy to find.
 * Where the ProtoEvaluationLink shows up in the incoming set of both
   SomeAtom, and of the PredicateNode, so that it is easy to find, given
   one or the other or both.
 * Perhaps there should be a ProtoStateLink, which would be globally
   unique, given the first two elements.
 * Perhaps the ProtoStateLink should be a real actual link, kept in the
   atomspace???

 * What is the *fastest* algorithm for finding the value, given SomeAtom,
   and the PredicateNode ... ? Perhaps the fastest algorithm would come
   from using this structure:
```
   ProtoEvaluationLink
         ListLink    # a real link, thus globally unique, and easily found in the atomspace.
             PredicateNode "name of the attribute, e.g. AV"
             SomeAtom
         FloatValue   # which is a vector of floats holding the actual av
```

The above ListLink variant uses more RAM...

My current favorite ideas are the above or something very similar;
there are certainly other ways to do things; do the other ways have an
equally simple API?
