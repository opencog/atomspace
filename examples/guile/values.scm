;
; values.scm
;
; Example of use `ProtoAtoms` aka `Values`, mixed with regular atoms.
; ProtoAtoms are similar to regular atoms, except that:
; 1) They do not have a TV or AV.
; 2) They cannot be placed in the AtomSpace.
; 2a) As a result they are not universally unique.
; 2b) They do not have a UUID.

(use-modules (opencog))

; Values can store vectors of floats ...
(define f (FloatValue 0.1 0.2 3.3 4.5678))

; or lists of strings:
(define s (StringValue "asdf" "gh" "jkl;"))

; or lists of other values or atoms.  Thus, they can be heirarchical.
(define l (LinkValue
  (Concept "foobar") (StringValue "property") (FloatValue 42)))

; Values can be attached to atoms:
(define a (Concept "some atom"))
(define k1 (ConceptNode "first key"))
(cog-set-value! a k1 f)

; The attached value can be fetched.
(cog-value a k1)

; The value can be changed ...
(cog-set-value! a k1 l)

; Verify that the value changed.
(cog-value a k1)

; Multipe values can be attached using different keys.
(define k2 (ConceptNode "second key"))
(cog-set-value! a k2 s)
(cog-value a k2)

; Verify that the value for the first key is still there.
(cog-value a k1)
