;
;
; several at once
;
(use-modules (opencog) (opencog persist) (opencog persist-rocks))

(define as-main (cog-atomspace))
(define as-one (AtomSpace "foo"))
(define as-two (AtomSpace "bar"))
(define as-three (AtomSpace "bing"))

(Edge (Predicate "bundle") (List (Item "AtomSpace Bundle Alpha") as-one))
(Edge (Predicate "bundle") (List (Item "AtomSpace Bundle Alpha") as-two))
(Edge (Predicate "bundle") (List (Item "Bundle Beta") as-three))

(cog-prt-atomspace)
(cog-set-atomspace! as-one)
(Concept "I am in as One!")
(cog-prt-atomspace)

(cog-set-atomspace! as-two)
(Concept "Resident of Two, here!")
(cog-prt-atomspace)

(cog-set-atomspace! as-three)
(EdgeLink (Predicate "three-ness") (Item "Just an old lump of coal"))
(cog-prt-atomspace)

(cog-set-atomspace! as-main)

(define rsn (RocksStorageNode "rocks:///tmp/bundle-demo"))
(cog-open rsn)
(store-frames as-one)
(store-atomspace as-one)
(store-frames as-two)
(store-frames as-three)
(store-atomspace as-two)
(cog-close rsn)

; -------------------------------------------------
(use-modules (opencog) (opencog persist) (opencog persist-rocks))

(define as-main (cog-atomspace))
(define rsn (RocksStorageNode "rocks:///tmp/bundle-demo"))
(cog-open rsn)
(load-frames)

(define as-one (AtomSpace "foo"))
(cog-set-atomspace! as-one)
(load-atomspace as-one)
(cog-prt-atomspace)
(cog-set-atomspace! as-one)
(cog-prt-atomspace)

(define as-two (AtomSpace "bar"))
(cog-set-atomspace! as-two)
(load-atomspace as-two)
(cog-prt-atomspace)

(cog-set-atomspace! as-one)
(load-atomspace as-one)
(cog-prt-atomspace)


