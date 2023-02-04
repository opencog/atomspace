;
; recursive-loop.scm - Writing tail-recursive loops.
;
; This demonstrates how to write a tail-recursive loop in Atomese.
;
; Tail-recursive loops in Atomese are easy. Perhaps too easy; thus
; this example adds a lot of pointless complication so as to provide
; a more realistic example.
;
; First, it defines a random-number source, attaches it to an atom.
; It then samples the random number three times, adds it together,
; and then tests to see if it is less than 2.5. If it is (and this
; will usually be the case) then the tail-recursion happens: the
; defined predicate calls itself.

(use-modules (opencog) (opencog exec))

; Create a stream of random-number values, between 0 and 1.
; See `stream.scm` for an extended demonstration of streams.
(define someplace (Concept "A"))
(define key (PredicateNode "*-uniform-*"))
(cog-set-value! someplace key (RandomStream 1))

; Define a predicate that tests to see if the sum of three random
; values is less than 2.5 or not.
(define uniform (FloatValueOfLink someplace key))
(Define (DefinedPredicate "keep going?")
   (GreaterThan (Number 2.5) (Plus uniform uniform uniform)))

; Print something.
(define (print-stuff) (display "hi there!\n") (stv 1 1))

; Define a tail-recursive loop.
(Define
   (DefinedPredicate "My tail-recursive loop")
   (SequentialAnd
      (DefinedPredicate "keep going?")
      (Evaluation (GroundedPredicate "scm: print-stuff") (List))
      (DefinedPredicate "My tail-recursive loop")
   ))

; This should print six times or so, maybe less, maybe more.
(cog-evaluate! (DefinedPredicate "My tail-recursive loop"))
