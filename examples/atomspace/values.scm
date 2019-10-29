;
; values.scm -- Attaching generic values on Atoms.
;
; The previous example, `truthvalues.scm`, showed how to assign
; sequences of floating-point numbers to Atoms. There is no need
; to limit oneself only to floats; there is a generic facility
; for attaching Values to Atoms.
;
; Why is this interesting? Why is this needed? Short answer: because
; its faster that way.
;
; In principle, you can put "anything" into the AtomSpace; it is quite
; generic. In practice, the AtomSpace forces certain assumptions that
; have strong impacts on runtime performance and on system RAM usage.
; In order to make Atoms pattern-matchable (searchable, queryable), the
; entire graph structure has to be kept. This uses a lot of RAM. To have
; database-like properties, the AtomSpace has to keep indexes of Atoms.
; This uses yet more RAM, and also makes Atom insertion and removal
; slow. If you do NOT need searchability, and you are concerned about
; performance, Values are for you!
;
; Values (such as TruthValues) live in a per-Atom key-value database.
; Given any Atom, and a Key, you can get the Value attached there.
; Given any Atom, Key and Value, you can quickly swap the new value for
; the old. Here's what you cannot do:
;
; 1) Use GetLink or BindLink to search for Values
; 2) Use PutLink to create new Values
; 3) Store them in the Atomspace for later retrieval. (but you can
;    store the Atom attache there; the Value will ride with it.)
;
; Values and Atoms do share a common type system: one can work with
; Value types in the same was as with Atom types. This includes using
; all of the various type constructors.
;
; Time for the examples:

(use-modules (opencog))

; Values can store vectors of floats ...
(define f (FloatValue 0.1 0.2 3.3 4.5678))

; ... or lists of strings:
(define s (StringValue "asdf" "gh" "jkl;"))

; ... or lists of other values or atoms.  Thus, they can be arranged
; in a hierarchical tree.
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

; Multiple values can be attached using different keys.
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
(use-modules (opencog attentionbank))
(cog-set-av! a (av 3 2 1))
(cog-keys a)

; ... and can also be accessed as values:
(define kav (PredicateNode "*-AttentionValueKey-*"))
(cog-value a kav)
(cog-av a)
(equal? (cog-value a kav) (cog-av a))
