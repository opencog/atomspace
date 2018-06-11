;
; Data set which the basic API will yoke together.

(define chicken-legs-pair (List (Word "chicken") (Word "legs")))
(define chicken-legs (Evaluation (Predicate "foo") chicken-legs-pair))
(cog-set-value! chicken-legs (Predicate "counter") (FloatValue 1 2 3))

*unspecified*
