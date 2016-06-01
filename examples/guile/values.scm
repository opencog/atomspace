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
(FloatValue 0.1 0.2 3.3 4.5678)

; or lists of strings:
(StringValue "asdf" "gh" "jkl;")

; or lists of other values or atoms.  Thus, they can be heirarchical.
(LinkValue
	(Concept "foobar")
	(StringValue "property")
	(FloatValue 42)
)

; Values can be attached to atoms:
(define a (Concept "some atom"))
(define v (LinkValue (StringValue "property") (FloatValue 42)))
(cog-set-value! a v)

; The attached value can be fetched.
(cog-value a)

(define l (LinkValue
	(Concept "foobar") (StringValue "property") (FloatValue 42)))

; The following should throw errors.
(cog-value v)
(cog-value l)

; The value can be changed ...
(cog-set-value! a l)

; Verify that the value changed.
(cog-value a)
