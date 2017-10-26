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

; A list of values can be converted into an ordinary scheme list:
(cog-value->list f)
(cog-value->list s)
(cog-value->list l)

; Alternately, individual elements in the list can be accessed directly.
; This behaves just like   (list-ref (cog-value->list VAL) NUM)
; but is computationally faster.
(cog-value-ref f 2)
(cog-value-ref s 0)
(cog-value-ref l 1)

; Help for these functions can be viewed at the guile command line:
,d cog-value->list
,d cog-value-ref

; Values can be attached to atoms:
(define a (Concept "some atom"))
(define k1 (PredicateNode "first key"))
(cog-set-value! a k1 f)

; The attached value can be fetched.
(cog-value a k1)

; The value can be changed ...
(cog-set-value! a k1 l)

; Verify that the value changed.
(cog-value a k1)

; Multipe values can be attached using different keys.
(define k2 (PredicateNode "second key"))
(cog-set-value! a k2 s)
(cog-value a k2)

; Verify that the value for the first key is still there.
(cog-value a k1)

; List all of the keys on the atom.
(cog-keys a)

; Add a truth value to the atom
(cog-set-tv! a (stv 0.9 0.8))

; List all of the keys on the atom.
; Note that the tv is stored with a key, so that truth values
; behave like other values.
(cog-keys a)

; Lets play with truth values. First, define the truth value key:
(define ktv (PredicateNode "*-TruthValueKey-*"))

; Verify that it works as expected; that is, this should return
; exactly the same thing as (cog-tv a)
(cog-value a ktv)
(cog-tv a)
(equal? (cog-value a ktv) (cog-tv a))

; Truth Values are values, just like the rest. So are Attention Values:
(define l2 (LinkValue
  (stv 0.1 0.2) (stv 0.3 0.4) (Concept "foobar") (av 3 2 1) (av 4 5 0)))

(cog-set-value! a k2 l2)
(cog-value a k2)

; Attention values are stored under a special key as well:
(cog-set-av! a (av 3 2 1))
(cog-keys a)

; and can be accessed as values:
(define kav (PredicateNode "*-AttentionValueKey-*"))
(cog-value a kav)
(cog-av a)
(equal? (cog-value a kav) (cog-av a))
