;
; Test composition of function with diagonal operator (coproduct)
; This triggers the alpha-conversion code path; its almost just
; a variable renaming, together with an eta conversion at the end,
; so that the vaialbe $Z becomes bound, instead of being free.
;
(define put-1
(Put
  (Lambda (Inheritance (Variable "$X") (Variable "$Y")))
  (List (Variable "$Z") (Variable "$Z"))))

(define expected-1
(LambdaLink
  (VariableNode "$Z")
  (InheritanceLink
    (VariableNode "$Z")
    (VariableNode "$Z"))))

; -----------------------------------------------------
; Similar to above, but with free variables, instead of bound vars.
; That is, both variables $X and $Y are free. The beta-substitution
; keeps them free, and does not bind them (no eta-conversion).
;
(define put-2
(Put
  (Inheritance (Variable "$X") (Variable "$Y"))
  (List (Variable "$Z") (Variable "$Z"))))

(define expected-2
  (InheritanceLink
    (VariableNode "$Z")
    (VariableNode "$Z")))

; -----------------------------------------------------
; Simple alpha conversion.
; All three expected results are alpha-equivalent; any one will do.
; All three should equal the result of evaluating the put.

(define put-3
(Put
  (Lambda
    (Inheritance (Variable "$X") (Variable "$Y")))
    (List (Variable "$Z") (Variable "$W"))))

(define expected-3
(LambdaLink
  (InheritanceLink
    (VariableNode "$X")
    (VariableNode "$Y"))))

(define expected-3-alt
(LambdaLink
  (InheritanceLink
    (VariableNode "$Z")
    (VariableNode "$W"))))

(define expected-3-alt-b
(LambdaLink
  (InheritanceLink
    (VariableNode "$foo")
    (VariableNode "$bar"))))

; -----------------------------------------------------
; Simple variable renaming of free variables.
; Free variables are never alpha-converted.
; Thus, the result should never be "unexpected-4"

(define put-4
(Put
  (Inheritance (Variable "$X") (Variable "$Y"))
  (List (Variable "$Z") (Variable "$W"))))

(define expected-4
(InheritanceLink
  (VariableNode "$Z")
  (VariableNode "$W")))

(define unexpected-4
(InheritanceLink
  (VariableNode "$X")
  (VariableNode "$Y")))

; -----------------------------------------------------
; Test conversion of putlink into prenex form.
; Naively, one might think that one should get
;    (List (Lambda stuff) (Concept "D"))
; as the result of the evaluation; in fact, the lambda gets
; pulled out, so that the final result is in prenex form.
;
(define put-5
(Put
  (List
    (Variable "$spe-arg-0")
    (Concept "D"))
  (Lambda
    (VariableList
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1")))))

(define expected-5
(Lambda
  (VariableList
    (Variable "$sha-arg-0")
    (Variable "$sha-arg-1"))
  (List
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Concept "D"))))

; -----------------------------------------------------
; Test conversion of putlink into prenex form.
; Naively, one might think that one should get
;    (List (Pattern stuff) (Concept "D"))
; as the result of the evaluation; in fact, the pattern gets
; pulled out, so that the final result is in prenex form.
; This is nearly identical to put-5 above, but with
; a non-lambda version of the PrenexLink
;
(define put-6
(Put
  (List
    (Variable "$spe-arg-0")
    (Concept "D"))
  (Pattern
    (VariableList
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1")))))

(define expected-6
(Pattern
  (VariableList
    (Variable "$sha-arg-0")
    (Variable "$sha-arg-1"))
  (List
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Concept "D"))))

; -----------------------------------------------------
; Test non-conversion to prenex form.
; One should get only
;    (List (Scope stuff) (Concept "D"))
; as the result of the evaluation; since the ScopeLink is NOT
; a prenex link, it should not get pulled out. Very nearly
; identical to put-5 and put-6, but not a prenex.
;
;
(define put-7
(Put
  (List
    (Variable "$spe-arg-0")
    (Concept "D"))
  (Scope
    (VariableList
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1")))))

(define expected-7
(List
  (Scope
    (VariableList
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
      (Inheritance
        (Variable "$sha-arg-0")
        (Variable "$sha-arg-1")))
   (Concept "D")))
