;; We have 3 balls, 2 explicitly green, and one implicitely green
;; since it absorbs only red and blue.
;;
;; We want to prove that all balls are green in a backward way, which
;; requires to infer that the third ball is in fact green.

;; Balls
(define B1 (Concept "B1"))
(define B2 (Concept "B2"))
(define B3 (Concept "B3"))

;; Classes
(define ball (Concept "ball"))
(define green (Concept "green"))
(define red_blue_absorb (Concept "red_blue_absorb"))

;; B1 to B3 are balls
(Inheritance (stv 1 1) B1 ball)
(Inheritance (stv 1 1) B2 ball)
(Inheritance (stv 1 1) B3 ball)

;; B1 and B2 are explicitely green
(Inheritance (stv 1 1) B1 green)
(Inheritance (stv 1 1) B2 green)

;; B3 only absorbs red and blue
(Inheritance (stv 1 1) B3 red_blue_absorb)

;; Whatever only absorbs red and blue is green
(ImplicationScope (stv 1 1)
  (TypedVariable X (Type "ConceptNode"))
  (Inheritance X red_blue_absorb)
  (Inheritance X green))
