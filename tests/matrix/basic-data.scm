;
; Data set which the basic API will yoke together.

; Create the high-level pair, given the low-level one.
(define (mkfoo WA WB) (Evaluation (Predicate "foo") (List (Word WA) (Word WB))))

; Set the count on the high-level pair
(define (setcnt LK VAL) (cog-set-value! LK (Predicate "counter") (FloatValue 1 2 VAL)))

(define chicken-legs (mkfoo "chicken" "legs"))
(setcnt chicken-legs 3)

; More data
(setcnt (mkfoo "chicken" "wings") 6)
(setcnt (mkfoo "chicken" "eyes") 2)
(setcnt (mkfoo "dog" "legs") 4)
(setcnt (mkfoo "dog" "snouts") 1)
(setcnt (mkfoo "dog" "eyes") 2)
(setcnt (mkfoo "table" "legs") 4)

; left-basis-size = 3 = chicken, dog, table
; right-basis-size = 4 = legs, wings, eyes, snouts
; total count = (+ 3 6 2 4 1 2 4) = 22

; chicken-support = 3
; dog-support = 3
; table=suport = 1
; average left support = (/ (+ 3 3 1) 3)

; N(chicken,*) = chicken-count = (+ 3 6 2) = 11
; N(dog,*) = dog-count = (+ 4 1 2) = 7
; N(table,*) = table-count = 4
; average left count = (/ (+ 11 7 4) 3)

; right supports: legs=3 eyes=2 wings=1 snouts=1
; right counts: legs=(+ 3 4 4)=11 eyes=(+ 2 2)=4 wings=6 snouts=1

; total support = (+ 3 3 1) = (+ 3 2 1 1) = 7
; total count = (+ 11 7 4) = (+ 11 4 6 1) = 22

; ---------------------------------------------
; N(*, legs) = 11
; N(*, eyes) = 4
; N(*, wings) = 6
; N(*, snouts) = 1
; sum_y N(chicken,y) N(*,y) = 3*11 + 6*6 + 2*4 = 77
; sum_y N(dog,y) N(*,y) = 4*11 + 2*4 + 1*1 = 53
; sum_y N(table,y) N(*,y) = 4*11 = 44
; sum sum mmt = 77 + 53 + 44 = 174
;
; sum_x N(x,legs) N(x,*) = 3*11 + 4*7 + 4*4
; sum_x N(x,eyes) N(x,*) = 2*11 + 2*7 + 0*4
; sum_x N(x,wings) N(x,*) = 6*11 + 0*7 + 0*4
; sum_x N(x,snouts) N(x,*) = 0*11 + 1*7 + 0*4
; sum sum = mtm = 77 + 36 + 66 + 7 = 186

*unspecified*
