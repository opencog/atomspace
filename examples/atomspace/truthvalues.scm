;
; truthvalues.scm -- Declaring the truth of a proposition.
;
; A knowledgebase records a set of facts. Or so it is claimed; but
; are all of these "facts" actually true?  The TruthValue provides
; an efficient, high-speed kind of way of associating a truth to
; any given Atom.
;
; TruthValues need not be boolean true/false crisp-logic values.
; They can be numbers, commonly interpreted as a probability. So,
; a truth value between zero and one is the "probability" of it
; being true. Sometimes, one can assign a probability, but without
; being confident that it is true: thus the most basic TruthValue
; is a SimpleTruthValue; it has a "strength" and a "confidence".
;
; Observational probabilities require counting to obtain a frequency.
; The CountTruthValue simplifies this representation.
;
; The AtomSpace is probability-theory agnostic: you can use TruthValues
; for Bayesian probabilities, frequentist probabilities, indefinite
; probabilities, or whatever you can imagine. None of the rules enforce
; any one particular theory.
;
; The word "Value" comes from the mathematical model-theoretic notion
; of a "valuation". In model theory, a valuation is a special kind of
; function that assigns a truth value to any proposition. Roughly
; speaking, valuations provide the bridge between syntax and semantics.
; They indicate one possible world out of many.
;
; There are other ways of describing possibility, probability and modal
; logic in the AtomSpace; this includes Contexts and contextual truth
; values. All that is for later examples; this example focuses on the
; most basic forms.
;
(use-modules (opencog))

; TruthValues normally consist of two floats:
; by convention, a "strength" and a "confidence".
(define tv (SimpleTruthValue 0.1 0.2))

; A truth value can be converted to a scheme list
(cog-value->list tv)

; Alternately, individual elements in the list can be accessed directly.
; This behaves just like   (list-ref (cog-value->list VAL) NUM)
; but is computationally faster.
(cog-value-ref tv 0)
(cog-value-ref tv 1)

; Help for these functions can be viewed at the guile command line:
,d cog-value->list
,d cog-value-ref

; TruthValues can be attached to atoms:
(define a (Concept "some atom"))
(cog-set-tv! a tv)

; The attached truth value can be fetched.
(cog-tv a)

; The value can be changed ...
(cog-set-tv! a (SimpleTruthValue 0.333 0.789))

; Verify that the value changed.
(cog-tv a)

; The CountTruthValue is a sequence of three values.
; By convention, these are
;   the probability p, with 0 < p < 1.
;   the entropy measured in bits, i.e. -log_2(p).
;   the count, an integer.
; Although the probability and entropy are obviously related, there is
; no way to get the probability from the count, without referencing
; some total count that has to be stored elsewhere. The CountTruthValue
; does not actually force the entropy to be log2 of the probability.

(cog-set-tv! a (CountTruthValue 1.0e-6 -19.9316 55))

; Attention Values are a different kind of sequence of floats
; that can be attached to an atom.  They require the attention-bank
; module to be loaded. Using them causes the attention bank to track
; the total amount of "attention" in the system. See documentation
; for details.
(use-modules (opencog attention-bank))
(cog-set-av! a (AttentionValue 3 2 1))

; The AttentionValue can be accessed as expected:
(cog-av a)

;--------------
;
; Unlike Atoms, there is no way to search for a particular TruthValue
; (nor a particular AttentionValue). In particular, these do not live
; in the AtomSpace. They are not universally unique; two different
; TruthValues really are independent of each-other. They do not have
; a unique hash or UUID.  And finally, you cannot assign a TruthValue
; to a TruthValue.
;
