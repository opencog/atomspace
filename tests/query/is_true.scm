;
; is_true.scm for IsTrueUTest.cxxtest
;

;; True atom with BoolValue
(cog-set-value! (Concept "A") (Predicate "*-TruthValueKey-*") (BoolValue #t))

;; False atom with BoolValue
(cog-set-value! (Concept "B") (Predicate "*-TruthValueKey-*") (BoolValue #f))

;; Empty atom
(Concept "C")

;; Query all concepts
(define query
  (Meet
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (Present (Variable "$C"))))

;; Query only true concepts
(define true-query
  (Meet
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (And
     (Present (Variable "$C"))
     (IsTrue (Variable "$C")))))
