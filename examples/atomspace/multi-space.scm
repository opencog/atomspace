;
; multi-space.scm -- Using multiple AtomSpaces at the same time.
;
; It is convenient to place different datasets into different
; AtomSpaces, and yet work with all of them at once (such as
; searching over all of them for some specific pattern.) This
; demo shows how this can be done.
;
; FYI, AtomSpaces can be marked read-only. This helps avoid the
; accidental corruption of the contents, when working with multiple
; AtomSpaces at once. See the `copy-on-write.scm` demo for setting
; and controlling the read-only flag.
;

(use-modules (opencog) (opencog exec))

; Create three AtomSpaces; two singletons, and a third that contains
; the first two.
(define a (AtomSpace))
(define b (AtomSpace))
(define c (AtomSpace a b))

; Switch over the the first AtomSpace.  All subsequent operations on
; Atoms apply to this AtomSpace.
(cog-set-atomspace! a)

; Note that calling `cog-set-atomspace!` returns the OLD atomspace,
; and not the current one.

; Place some data into the AtomSpace.
(Concept "I'm in A")

; Print the contents of the AtomSpace.
(cog-prt-atomspace)

; Switch over to the second AtomSpace, and put some data into it.
(cog-set-atomspace! b)
(Concept "I'm in B")
(cog-prt-atomspace)

; Switch to the third AtomSpace.
(cog-set-atomspace! c)

; Note that it contains everythg in the first two.
(cog-prt-atomspace)

; Define a query that will look for all Concepts
(define get-concepts
	(Get (TypedVariable (Variable "$x") (Type 'Concept))
		(Variable "$x")))

; Take a look.
(cog-prt-atomspace)

; Run the query. It should find both Concepts. This is, it should
; print out a `SetLink` containing both of the above Concepts.
(cog-execute! get-concepts)

; Take a look.
(cog-prt-atomspace)

; Go back to the first AtomSpace. Verify that it is as we left it.
(cog-set-atomspace! a)
(cog-prt-atomspace)

; Go to the second AtomSpace, and try to run the query. An error should
; result, because the query is only defined in the union AtomSpace, and
; not in any of the components.
(cog-set-atomspace! b)
(cog-prt-atomspace)
(cog-execute! get-concepts)

; That's all folks! The End.
