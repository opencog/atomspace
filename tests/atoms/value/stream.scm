;
; Example of using values that are time-varying streams of data.
; Here, the stream is modelled by the `RandomStream`.
; Every time that it is accessed, it generates a different set
; of random floating-point values.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))

; First, define a key and an atom, and attach an ordinary value
; to the atom, for the given key.
(define k (Predicate "key"))
(define a (Concept "my atom"))
(cog-set-value! a k (FloatValue 1 2 3))

; Fetch the value, using the FloatValueOfLink
(define vo (FloatValueOf a k))
; (cog-execute! vo)

; Numeric computations can be performed on that value...
(define tym (Times (Plus vo (Number 6 6 6)) (Number 2)))
; (cog-execute! tym)

; Now, do it again, with the RandomStream
; The argument to RandomStream is how many random floats it
; should generate, per call. Here, its set to 24.
(define b (Concept "other atom"))
(cog-set-value! b k (RandomStream 24))

; Every call, the stream generates 24 numbers between 0 and 1.
; Add 6 to get a number between 6 and 7.
; Then multiply by 2, to get a number between 12 and 14.
(define sixes (make-list 24 6))
(define tymb (Times (Number 2) (Plus (FloatValueOf b k) (Number sixes)) ))
; (cog-execute! tymb)
