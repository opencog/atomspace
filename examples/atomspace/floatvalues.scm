;
; floatvalues.scm -- Declaring the probability/truth of a proposition.
;
; A knowledgebase records a set of facts. Or so it is claimed; but
; are all of these "facts" actually true?  The FloatValue provides
; an efficient, high-speed kind of way of associating a numerical truth
; to any given Atom.
;
; BoolValues provide crisp boolean on/off true/false 0/1 valuations.
; FloatValues are useful for indicating probabilities. Sometimes, it
; is convenient to record more than just a single number: perhaps one
; wants to indicate a probability and a confidence. FloatValues can
; do this: they record a vector of numbers. There is no particular limit
; to the size of this vector; it can be, for example, 8192 elements
; long, and hold the weights of some deeplearning neural net model.
;
; The word "Value" comes from the mathematical model-theoretic notion
; of a "valuation". In model theory, a valuation is a special kind of
; function that assigns a truth value to any proposition. Roughly
; speaking, valuations provide the bridge between syntax and semantics.
; They indicate one possible world out of many. The Atomese notion of
; a Value is inspired by this idea, but it is generalized based on
; practical needs.
;
; There are many kinds of Values in the Atomese system. Two prominent
; ones are StringValue, which can hold a vectors of strings, and
; LinkValue, which holds vectors of Values. This introductory demo
; focuses on FloatValues.
;
(use-modules (opencog))

; FloatValues are vectors of double-precision floating point numbers.
(define tv (FloatValue 0.1 0.2))

; A float value can be converted to a scheme list
(cog-value->list tv)

; Alternately, individual elements in the list can be accessed directly.
; This behaves just like   (list-ref (cog-value->list VAL) NUM)
; but is computationally faster.
(cog-value-ref tv 0)
(cog-value-ref tv 1)

; Help for these functions can be viewed at the guile command line:
,d cog-value->list
,d cog-value-ref

; FloatValues can be attached to atoms. The attachment point is
; indicated with a key.
(define a (Concept "some atom"))
(define k (Predicate "may favorit key"))
(cog-set-value! a k tv)

; The attached float value can be fetched.
(cog-value a k)

; The value can be changed ...
(cog-set-value! a k (FloatValue 0.333 0.789))

; Verify that the value changed.
(cog-value a k)

;--------------
;
; Unlike Atoms, there is no way to search for a particular FloatValue
; In particular, these do not live in the AtomSpace. They are not
; universally unique; two FloatValues really are independent of each
; other, even if they hold the same numerical value. They do not have
; a unique hash or UUID.  And finally, you cannot assign a FloatValue
; to a FloatValue.
;
; The End! That's all, Folks!
