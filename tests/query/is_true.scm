;
; is_true.scm for IsTrueUTest.cxxtest
;

;; True atom
(Concept "A" (stv 1 1))

;; Non-true atom
(Concept "B")

;; Query all concepts
(define query
  (Get
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (Present (Variable "$C"))))

;; Query only true concepts
(define true-query
  (Get
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (And
     (Present (Variable "$C"))
     (IsTrue (Variable "$C")))))
