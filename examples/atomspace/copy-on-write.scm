 ;
 ; Example of copy-on-write into overlay AtomSpaces.
 ;
 ; The goal is to demonstrate how to use read-write atomspaces
 ; layered on top of a read-only atomspace.
 ;
 ; This creates two atomspaces: the base atomspace, which
 ; will be marked read-only (after adding atoms to it),
 ; and an overlay atomspace, which will remain read-write.
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

 ; Create atoms in the base atomspace.
 (define a (Concept "a"))
 (define b (Concept "b"))

 (cog-set-tv! a (cog-new-stv 0.1 0.1))

 ; Mark the current atomspace as read-only.
 ; New atoms can no longer be created.
 (cog-atomspace-ro!)
 (define c (Concept "c"))
 (cog-prt-atomspace)

 ; Truth values can no longer be changed.
 (cog-set-tv! a (cog-new-stv 0.2 0.2))
 (cog-prt-atomspace)
 ; Create an overlay (that will be read-write)
 ;
 (define base (cog-atomspace))
 (define ovly (cog-new-atomspace base))
 (cog-set-atomspace! ovly)

 ; Alter the TV on atom a -- this causes a copy-on-write (COW)
 ; into the overlay atomspace.
 (cog-set-tv! a (cog-new-stv 0.3 0.3))
 (cog-prt-atomspace)
 (cog-set-tv! a (cog-new-stv 0.4 0.4))
 (cog-prt-atomspace)

 ; But in the base atomspace, we still have the original.
 (cog-set-atomspace! base)
 (cog-prt-atomspace)

 ; And the overlay still contains the modified atom.
 (cog-set-atomspace! ovly)
 (cog-prt-atomspace)

 ; Verify Links in the overlay
 (OrderedLink a b)  ; note that a is in the base
 (define averly (Concept "a")) ; get the overlay version
 (ListLink averly b)
 (cog-prt-atomspace)

