;
; Data set which the basic API will yoke together.

; Create the high-level par, given the low-level one.
(define (mkfoo PR) (Evaluation (Predicate "foo") PR))
(define (mkfoob WA WB) (Evaluation (Predicate "foo") (List (Word WA) (Word WB))))
; Set the count on the high-level pair
(define (setcnt LK VAL) (cog-set-value! LK (Predicate "counter") (FloatValue 1 2 VAL)))

(define chicken-legs-pair (List (Word "chicken") (Word "legs")))
(define chicken-legs (mkfoo chicken-legs-pair))
(setcnt chicken-legs 3)

; More data
(setcnt (mkfoob "chicken" "wings") 6)
(setcnt (mkfoob "chicken" "eyes") 2)
(setcnt (mkfoob "dog" "legs") 4)
(setcnt (mkfoob "dog" "snouts") 1)
(setcnt (mkfoob "dog" "eyes") 2)
(setcnt (mkfoob "table" "legs") 4)

; left-basis-size = 3 = chicken, dog, table
; right-basis-size = 4 = legs, wings, eyes, snouts
; total count = (+ 3 6 2 4 1 2 4) = 22

*unspecified*
