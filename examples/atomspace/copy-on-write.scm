;
; copy-on-write.scm -- Copy-on-write into overlay AtomSpaces.
;
; This demonstrates how to create a pair of AtomSpaces, one layered
; on the other, so that changes are limited to the upper layer, leaving
; the base layer undisturbed.  There are two distinct ways of doing
; this. One is to explicitly mark the base layer as read-only. The
; other is to mark the overlay copy-on-write (while leaving the base
; writable.)
;
; There are several expected use cases. The copy-on-write mechanism
; provides a very simple way to create "scratch" or temporary spaces,
; where calculations can be performed, without altering the base space.
;
; The read-only mechanism is a primitive permission system, and can be
; used to implement shared use. One example would be a large read-only
; database holding data that is shared by many people. Individual users
; can then "mount" this database, and make local changes to it (i.e.
; perform read-write operations) without corrupting the underlying
; shared read-only database.
;
; A more advanced use is to use deep stacks of AtomSpaces to hold
; partial results arising from long computations. These can be thought
; of as a historical record or a collection of Kripke frames of the
; calculations, with each level consisting of all changes between that
; and the previous level: a "changeset", similar to that in git.
; Multiple branches are also allowed, and so fancy algorithms can
; perform "many-worlds" computations, and directly compare AtomSpaces
; in different branches.
;
; By default, whenever a new AtomSpace is created as a layer on top of
; another, it is marked as a copy-on-write space. This flag can be
; cleared: in this case, the overlay becomes a write-through AtomSpace.
; Newly-created Atoms will still be created in the overlay, but changes
; to Values and TruthValues of Atoms in the base space will pass through,
; to the base space. This demo does not show the write-through ability;
; the user is encourage to explore this themselves.
;
(use-modules (opencog))

; Create atoms in the base AtomSpace.
(define a (Concept "a"))
(define b (Concept "b"))

(cog-set-tv! a (cog-new-stv 0.1 0.1))

; --- First, the read-only part of the demo.
; Mark the current AtomSpace as read-only.
; New atoms can no longer be created. (The atom `c` is not actually
; created.)
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

; Alter the TV on atom `a` -- this causes a copy-on-write (COW)
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
(OrderedLink a b)  ; note that `a` is in the base
(define averly (Concept "a")) ; get the overlay version
(ListLink averly b)
(cog-prt-atomspace)

; --- Next, the copy-on-write part of the demo.
; Let's restore the base to being read-write (thus unprotected)
; but mark the overlay as explicitly copy-on-write.
(cog-atomspace-rw! base)
(cog-atomspace-cow! #t ovly)

; Verify that atoms in the base space can now be modified.
(cog-set-atomspace! base)
(cog-set-tv! a (cog-new-stv 0.222 0.222))
(cog-prt-atomspace)

; Go back to the overlay, and verify that it has not been affected.
(cog-set-atomspace! ovly)
(cog-prt-atomspace)

; Change the TV of `a` in the overlay.
(cog-set-tv! a (cog-new-stv 0.5 0.5))
(cog-prt-atomspace)

; And a grand finale: change the TV of `b`. This will result in a
; new `b` being created in the overlay, with the new TV, while the
; original `b` remains in the base, as it was, unchanged.
(cog-set-tv! b (cog-new-stv 0.6 0.6))
(cog-prt-atomspace)

; Verify that the COW-ed' `b` is not in the base.
(cog-set-atomspace! base)
(cog-prt-atomspace)

; Create a ListLink in the base, with a non-default TV.
; While we're at it, change the TV on `b` as well.
(ListLink a b (stv 0.7 0.7))
(cog-set-tv! b (cog-new-stv 0.8 0.8))
(cog-prt-atomspace)

; Verify that the ListLink in the overlay has the default TV.
; Notice that the ListLink in the overlay was composed with the
; version of `b` that was in the base, and so the TV on that has
; changed.
(cog-set-atomspace! ovly)
(cog-prt-atomspace)
