;
; copy-on-write.scm -- Copy-on-write into overlay AtomSpaces.
;
; The goal is to demonstrate how to use read-write AtomSpaces
; layered on top of a read-only AtomSpace.
;
; This creates two AtomSpaces: the base AtomSpace, which
; will be marked read-only (after adding atoms to it),
; and an overlay AtomSpace, which will remain read-write.
; Atoms and truth values can be manipulated in the overlay,
; without damaging atoms in the base space.
;
; The expected use case is a very-large read-only database
; holding data that is shared by many people. Individual users
; can then "mount" this database, and make local changes to it
; (i.e. perform read-write operations) without corrupting the
; shared read-only database.
;
(use-modules (opencog))

; Create atoms in the base AtomSpace.
(define a (Concept "a"))
(define b (Concept "b"))

(cog-set-tv! a (cog-new-stv 0.1 0.1))

; Mark the current AtomSpace as read-only.
; New atoms can no longer be created.
(cog-atomspace-ro!)
(define c (Concept "c"))
(cog-prt-atomspace)

; Truth values can no longer be changed.
(cog-set-tv! a (cog-new-stv 0.2 0.2))
(cog-prt-atomspace)

; Create an overlay (that will be read-write)
(define base (cog-atomspace))
(define ovly (cog-new-atomspace base))
(cog-set-atomspace! ovly)

; Alter the TV on atom a -- this causes a copy-on-write (COW)
; of atom a into the overlay atomspace.
(cog-set-tv! a (cog-new-stv 0.3 0.3))
(cog-prt-atomspace)
(cog-set-tv! a (cog-new-stv 0.4 0.4))
(cog-prt-atomspace)

; The base atomspace still holds the original, unmodified atom.
(cog-set-atomspace! base)
(cog-prt-atomspace)

; The overlay contains only the modified atom.
(cog-set-atomspace! ovly)
(cog-prt-atomspace)

; Create a Link, and verify that it sits in the overlay.
; Pay attention to where the atoms in the Link sit: some will be
; in the base space, and some will be in the overlay. Which is where
; depends on how each scheme symbol got bound to an atom. This can
; be a source of confusion.
;
(OrderedLink a b)  ; note that a is in the base
(define averly (Concept "a")) ; get the overlay version
(ListLink averly b)
(cog-prt-atomspace)
