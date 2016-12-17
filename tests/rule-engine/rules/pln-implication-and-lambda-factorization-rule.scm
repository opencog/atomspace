;; =======================================================================
;; ImplicationLink AndLink Lambda Factorization Rule
;;
;; TODO: Replace this by higher order fact
;;
;; AndLink
;;    LambdaLink
;;       V
;;       A1
;;    ...
;;    LambdaLink
;;       V
;;       An
;; |-
;; LambdaLink
;;    V
;;    AndLink
;;       A1
;;       ...
;;       An
;;
;; where V is a variable or a list of variables, A1 to An are bodies
;; using containing variable(s) V.
;;
;; Also, the consequent will actually be
;;
;; ImplicationLink <1 1>
;;    AndLink
;;       LambdaLink
;;          V
;;          A1
;;       ...
;;       LambdaLink
;;          V
;;          An
;;    LambdaLink
;;       V
;;       AndLink
;;          A1
;;          ...
;;          An
;;
;; Because it is much easier to chain later on. This will be replaced
;; by higher order facts later.
;; -----------------------------------------------------------------------

(define implication-and-lambda-factorization-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs-one")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableNode")
           (TypeNode "VariableList")))
     (TypedVariableLink
        (VariableNode "$TyVs-two")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableNode")
           (TypeNode "VariableList")))
     (VariableNode "$A1")
     (VariableNode "$A2")))

(define implication-and-lambda-factorization-body
  (LocalQuoteLink                        ; Necessary so the AndLink doesn't
                                         ; count as a connective
     (AndLink
        (QuoteLink (LambdaLink
           (Unquote (VariableNode "$TyVs-one"))
           (Unquote (VariableNode "$A1"))))
        (QuoteLink (LambdaLink
           (Unquote (VariableNode "$TyVs-two"))
           (Unquote (VariableNode "$A2")))))))

(define implication-and-lambda-factorization-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: implication-and-lambda-factorization-formula")
     (ListLink
        (VariableNode "$TyVs-one")
        (VariableNode "$TyVs-two")
        (VariableNode "$A1")
        (VariableNode "$A2"))))

(define implication-and-lambda-factorization-rule
  (BindLink
     implication-and-lambda-factorization-variables
     implication-and-lambda-factorization-body
     implication-and-lambda-factorization-rewrite))

(define (implication-and-lambda-factorization-formula var1 var2 a1 a2)
  (let ((and-lamb (AndLink (LambdaLink var1 a1) (LambdaLink var2 a2)))
        (lamb (LambdaLink (Variable "$flat") (cog-new-flattened-link 'AndLink a1 a2))))
    (cog-set-tv! lamb (cog-tv and-lamb))
    (cog-set-tv! (ImplicationLink and-lamb lamb) (stv 1 1))))

;; Name the rule
(define implication-and-lambda-factorization-rule-name
  (DefinedSchemaNode "implication-and-lambda-factorization-rule"))
(DefineLink implication-and-lambda-factorization-rule-name
  implication-and-lambda-factorization-rule)
