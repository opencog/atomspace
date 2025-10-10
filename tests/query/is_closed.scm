;
; is_closed.scm for IsClosedUTest.cxxtest
;

;; Closed atom
(Inheritance
  (Concept "A")
  (Concept "B"))

;; Open atom
(Inheritance
  (Concept "A")
  (Variable "$freevar"))

;; Query all inheritance links
(define query
  (Meet
    (TypedVariable
      (Variable "$I")
      (Type 'Inheritance))
    (Present (Variable "$I"))))

;; Query only inheritance links
(define closed-query
  (Meet
    (TypedVariable
      (Variable "$I")
      (Type 'Inheritance))
    (And
     (Present (Variable "$I"))
     (IsClosed (Variable "$I")))))
