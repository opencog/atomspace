;
; is_false.scm for IsFalseUTest.cxxtest
;

;; False atom
(Concept "A" (stv 0 1))

;; Non-false atom
(Concept "B")

;; Query all concepts
(define query
  (Get
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (Present (Variable "$C"))))

;; Query only false concepts
(define false-query
  (Get
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (And
     (Present (Variable "$C"))
     (IsFalse (Variable "$C")))))
