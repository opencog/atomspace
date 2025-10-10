;
; is_false.scm for IsFalseUTest.cxxtest
;

;; False atom with BoolValue
(cog-set-value! (Concept "A") (Predicate "*-TruthValueKey-*") (BoolValue #f))

;; True atom with BoolValue
(cog-set-value! (Concept "B") (Predicate "*-TruthValueKey-*") (BoolValue #t))

;; Empty atom
(Concept "C")

;; Query all concepts
(define query
  (Meet
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (Present (Variable "$C"))))

;; Query only false concepts
(define false-query
  (Meet
    (TypedVariable
      (Variable "$C")
      (Type 'Concept))
    (And
     (Present (Variable "$C"))
     (IsFalse (Variable "$C")))))
