;
; basic.scm -- Basic guile usage example!
;
; See `opencog/guile/README` or http://wiki.opencog.org/w/Scheme
; for additional documentation.
;
; The AtomSpace guile module is called `opencog`, and not `atomspace`,
; as you might expect. This is a historical artifact, meant to preserve
; backwards compatibility with the earliest versions of OpenCog. Thus,
; you will see the string "opencog" litering the code, everywhere.
; Wherever you see it, keep in mind that it refers to the AtomSpace.
; All OpenCog modules are always AtomSpace sub-modules. There is nothing
; in the OpenCog project that can work without the AtomSpace!
;
; If you have installed this git repo with `make install`, then start
; guile simply by saying `guile` at the bash prompt `$`.  Otherwise,
; you will need to do this:
;
;    $ guile -L opencog/scm -L build
;
; where `build` is where-ever you built this git repo.
;
; Another possibility: add paths to your `~/.guile` file. For example:
;
;   (add-to-load-path "/home/yourname/atomspace/opencog/scm")
;

; Load the base module for the AtomSpace. Yes, it is called "opencog".
; This is a historical artifact; see above.
(use-modules (opencog))

; Create a Node and place it in the default AtomSpace.
(ConceptNode "asdf")

; Short names also work:
(Concept "foo")

; Links are lists of Atoms (which can be Nodes or Links).
(ListLink (Concept "foo") (Concept "bar"))
(ListLink (Concept "foo") (ListLink (Concept "bar") (Concept "bar")))

; Atoms can be accessed in such a way that they are not created if
; they do not already exist. Access the above:
(cog-node 'ConceptNode "asdf")

; Access an atom that does not exist. This will return the empty list.
(cog-node 'ConceptNode "qwerty")
(cog-link 'ListLink (cog-node 'ConceptNode "Oh no!"))

; All Atoms are ordinary scheme objects, and so conventional scheme
; can be used to hold references to them:
(define f (Concept "foo"))
f
(format #t "Let us print it out: ~A\n" f)
(define fff (ListLink f f f))
fff
(format #t "Here is a bunch: ~A\n" fff)

; The single quote-mark in front of 'ConceptNode means that it is a
; scheme symbol, and not a string:
(symbol? 'foo)
(symbol? "bar")

; The ConceptNode is an Atom type. All Atom types are "types" in
; the mathematical sense of "Type Theory". More plainly, this is
; more-or-less the same thing as a "type" in ordinary programming
; languagues. Atomese is a typed language. All Atom types have a
; matching C++ class that runs "under the covers". The Atom type
; inheritance hierarchy runs more-or-less the same as the C++ class
; hierarchy; there are some subtle exceptions.
;
; Get a list of all Atomese types:
(cog-get-types)

; In the above, note that the type hierarchy begins with `Value` and
; not with `Atom`. This distinction is important: Atoms can be stored
; in the AtomSpace, Values cannot. Atoms are globally unique; there
; can only ever be one instance of (Concept "asdf"), and every such
; reference always refers to the same Atom. Because Atoms are held in
; the AtomSpace, they continue to exist even if all scheme references
; to them are dropped. The AtomSpace maintains a reference to prevent
; them from being deleted.
;
; By contrast, a new Value is created (instantiated) every time it
; is mentioned; Values are always destroyed (garbage-collected) when
; there are no more references to them.
(FloatValue 0 1 2 3.14159)

; As the above shows, most Values are vectors. This is a storage-space
; and access-speed optimization. Later examples show how elements in the
; vector can be accessed and manipulated.

; Values can be attached to Atoms, using a key (as key-value pairs).
; Effectively, all Atoms are *also* key-value pair databases. Later
; examples explore this idea more deeply.
(cog-set-value! (Concept "asdf") (Predicate "some key") (FloatValue 4 5 6))
(cog-value (Concept "asdf") (Predicate "some key"))

; The guile REPL shell has built-in commands that assist with
; documentation. For example, the following will print a list of
; all functions having the string "cog" in them. Note the comma
; in front; the REPL-shell commands all start with a comma.
,apropos cog

; The above can be shortened to just ,a:
,a cog

; Get the documentation of a a given function
,describe cog-new-node
,describe cog-node

; The above can also be shortened:
,d cog-link
