
The Guile Scheme API
====================
Linas Vepstas, May 2008
Rvision of December 2025


This directory contains (Guile) Scheme bindings to Atomese. These allow
Atomese and Scheme to be freely mixed, providing a convenient way of
boostrapping Atomese projects.

The bindings are implemented via
[guile](http://www.gnu.org/software/guile/guile.html); see the
[guile reference manual](http://www.gnu.org/software/guile/manual/)
One of the finest books ever written about programming is the SICP
book, ["Structure and Interpretation of Computer
Programs"](http://mitpress.mit.edu/sicp/full-text/book/book.html),
by Abelson and Sussman. Although the examples are all in scheme,
its an excellent way of learning programming in any language at all.

Atoms can be created as simply as:
```
   (Reference
       (Concept "dog-instance-1") (Word "dog"))
```
which creates a link connecting a `ConceptNode` and a `WordNode`.
Any valid Node or Link name can be used in this way. (Note, however,
neither `ReferenceLink` nor `WordNode` are part of the base module.

Load the module by saying `(use-modules (opencog))`.

An expaneded, improved version of this README can be found on the wiki:
http://opencog.org/wiki/Scheme

== Running ==
There are multiple ways to access the scheme bindings:

* By directly running the guile REPL
* By starting the cogserver, and telneting to port 17001

== On-line documentation ==
Documentation for all routines and utilities can be pulled up at the
guile shell prompt, by saying <tt>,d name-of-proc</tt> or
<tt>(display (procedure-documentation name-of-proc))</tt>.


== Modules ==
Various different OpenCog projects define additional modules.
Last count, there were more than a dozen that were actively maintained.
The two main modules provided here are `(opencog)` and `(opencog logger)`.

== Core Functions ==
The following are the "core" functions, implemented in the guile-to-C++
interface.

=== cog-new-node node-type node-name ===
Create a new node of the given type and name

Throws errors if node-type is not a valid atom type for a node,
and if node-name is not a string.

Example:
    ; Create a new node, and prints its value:
    guile> (cog-new-node 'ConceptNode "some node name")
    (ConceptNode "some node name")


=== cog-node node-type node-name ===
Returns the node of the given type and name, if it exists, else
returns null.

Throws errors if node-type is not a valid atom type for a node,
and if node-name is not a string.

Example:
    ; Check to see if a node exists:
    guile> (cog-node 'ConceptNode "asdf")
    ()

    ; Verify that the return value is actually a true null:
    guile> (null? (cog-node 'ConceptNode "asdf"))
    #t

    ; Now, create the node, and see if it exists:
    guile> (cog-new-node 'ConceptNode "asdf")
    (ConceptNode "asdf")
    guile> (null? (cog-node 'ConceptNode "asdf"))
    #f


=== cog-new-link link-type atom ... atom ===
Create a new link, with the given atoms in the link.

Throws errors if the link type is not a valid opencog link type,
or if any of the arguments after the link type are not atoms.

Example:
    ; Creates two nodes, and a new link:
    guile> (define x (cog-new-node 'ConceptNode "abc"))
    guile> (define y (cog-new-node 'ConceptNode "def"))
    guile> (cog-new-link 'Link x y)
    (Link
       (ConceptNode "abc")
       (ConceptNode "def")
    )


=== cog-link link-type atom ... atom ===
Returns the link of the given type and list of atoms, if it
exists, else returns null.

Throws errors if the link type is not a valid opencog link type,
or if any of the arguments after the link type are not atoms.

Example:
    ; Create two nodes:
    guile> (define x (cog-new-node 'ConceptNode "abc"))
    guile> (define y (cog-new-node 'ConceptNode "def"))

    ; Does a node with these two links exist?
    guile> (cog-link 'Link x y)
    ()

    ; Now, create such a link
    guile> (cog-new-link 'Link x y)
    (Link
       (ConceptNode "abc")
       (ConceptNode "def")
    )

    ; Check again for existence:
    guile> (cog-link 'Link x y)
    (Link
       (ConceptNode "abc")
       (ConceptNode "def")
    )

=== cog-extract atom ===
Delete the indicated atom, but only if it has no incoming links.

Returns #t if the atom was deleted, else returns #f if not deleted.

=== cog-extract-recursive atom ===
Delete the indicated atom, and all atoms that point at it.

Both functions return #t on success, else they return #f.
If #f is returned, then the delete failed.

Example:
   ; Define two nodes and a link between them:
   guile> (define x (ConceptNode "abc"))
   guile> (define y (ConceptNode "def"))
   guile> (define l (Link x y))

   ; Verify that there's an atom called x:
   guile> x
   (Link
      (ConceptNode "abc")
      (ConceptNode "def")
   )

   ; Try to delete x. This should fail, since there's a link
   ; containing x.
   guile> (cog-extract x)
   #f

   ; Delete x, and everything pointing to it. This should delete
   ; both x, and the link l.
   guile> (cog-extract-recursive x)
   #t

   ; Verify that the link l is gone:
   guile> l
   Invalid handle

   ; Verify that the node x is gone:
   guile> x
   Invalid handle

   ; Verify that the node y still exists:
   guile> y
   (ConceptNode "def")


=== cog-atom? exp ===
Return #t if exp is an atom, else return #f

Example:
   ; Define a node
   guile> (define x (ConceptNode "abc"))
   guile> (define y (+ 2 2))
   guile> (cog-atom? x)
   #t
   guile> (cog-atom? y)
   #f


=== cog-name atom ===
Return the name of the node. If the atom is not a node,
returns NIL.

Example:
   ; Define a node
   guile> (define x (ConceptNode "abc"))
   guile> (cog-name x)
   "abc"


=== cog-type atom ===
Return the type of the atom.

Example:
   ; Define a node
   guile> (define x (ConceptNode "abc"))
   guile> (cog-type x)
   ConceptNode
   guile> (eq? 'ConceptNode (cog-type x))
   #t

=== cog-incoming-set atom ===
Return the incoming set of the atom.
Returned as ordinary scheme list.

Example:
   ; Define two nodes and a link between them:
   guile> (define x (ConceptNode "abc"))
   guile> (define y (ConceptNode "def"))
   guile> (define l (Link x y))

   ; Get the incoming sets of nodes x and y (which is the link l):
   guile> (cog-incoming-set x)
   ((Link
      (ConceptNode "abc")
      (ConceptNode "def")
   )
   )

   guile> (cog-incoming-set y)
   ((Link
      (ConceptNode "abc")
      (ConceptNode "def")
   )
   )

   ; Verify that the both incoming sets are really one and the
   ; same link:
   guile> (equal? (cog-incoming-set x) (cog-incoming-set y))
   #t

   ; The returned values are lists, and not singleton atoms.
   ; Thus, the incoming set of x is a list containing l:
   guile> (equal? (cog-incoming-set x) (list l))
   #t

   ; Verify that the returned value is a true list:
   guile> (list? (cog-incoming-set x))
   #t


=== cog-handle atom ===
Return a 64-bit hash of the atom. This can be used in hashing
functions.

It may be useful to remember that scheme indicates hexadecimal
numbers by preceding them with #x, and so, for example,
(cog-atom #x2c949b) gets the handle associated with hex 2c949b.

Example:
   ; Create two atoms, and get their handles:
   guile> (define x (ConceptNode "abc"))
   guile> (define y (ConceptNode "def"))
   guile> (cog-handle x)
   999732144055296198
   guile> (cog-handle y)
   7856230993510549357

=== cog-get-types ===
Return a list of all of the atom types in the system.

Example:
    guile> (cog-get-types)

=== cog-get-subtypes type ===
Return a list of the subtypes of the given type.  Only the
immediate subtypes are returned; to obtain all subtypes, this
function should be called recursively.

Example:
    guile> (cog-get-subtypes 'Atom)
    (Link Node)

=== cog-subtype? type subtype ===
Return #t if 'subtype' is a subtype of 'type', else return #f.
The check is performed recursively.

Example:
    guile> (cog-subtype? 'Node 'Link)
    #f
    guile> (cog-subtype? 'Atom 'Link)
    #t
    guile> (cog-subtype? 'Atom 'ConceptNode)
    #t

=== Ad Hoc commands ===
The scheme module provides a generic mechanism to declare new
scheme "primitive functions" ''i.e.'' functions which call methods
on a C++ object. New primitives may be defined by  using the
"define_scheme_primitive" function in "opencog/guile/SchemePrimitive.h".

This is a very powerful and generic tool that can be used to create
scheme wrappers around anything Atomese related. However, it is also
strongly discouraged: the current idea is that you should be coding
in Atomese, natively, and not in scheme (or python, for that matter).
Thus, the proliferation of modules and functions that got auto-wrapped
by SchemePrimitve are being actively hunted down and whacked. You may
yet find some, but they are increasingly rare. Shame, because it was
a cute, elegant trick.


=Notes, examples, utilities=
Various related notes.

===Calling scheme from Atomese===
The `ExecutionOutputLink`, whem used with the
`GroundedSchemaNode`, allows scheme code to be invoked when the
given `ExecutionOutputLink` is executed.  See the wiki page for details.

=== Calling scheme from C++ ===
Scheme code can be called from C++. The most common case is handled
below: calling a scheme function which expects 0,1,2 or more
<tt>Handle</tt> arguments, and returns a <tt>Handle</tt>. So, for
example, assume you have a scheme function called <tt>do-stuff</tt> as
below:

  (define (do-stuff handle1 handle2) (... return some-hand ...))

Then you can call it from C++ as follows:

  // Create a LIST_LINK on stack, holding the input arguments.
  Handle arg1 = ...;
  Handle arg2 = ...;
  hlnk = getAtomSpace.addLink(LIST_LINK, arg1, arg2, ...);

  // Call the "do-stuff" function
  SchemeEval evaluator;
  Handle answer = evaluator.apply("do-stuff", hlnk);

If <tt>do-stuff</tt> doesn't take any arguments, then set <tt>hlink</tt>
to <tt>Handle::UNDEFINED</tt>, or pass an empty `ListLink`.

The above form is used by the `ExecutionOutputLink` to embed scheme
callouts into hypergraphs.

=== Calling C++ from scheme ===
The C++ programmer may define new scheme primitive functions by using
the "define_scheme_primitive" function in "opencog/guile/SchemePrimitive.h".
This function expects a string that will be the scheme name, a C++ method,
and a pointer to a C++ object instance.  Unusual C++ signatures may require
extending SchemePrimitive.h, but this should not be hard.

The example program PrimitiveExample.cc shows how.
