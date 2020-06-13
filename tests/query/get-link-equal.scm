;;
;; get-link-equal.scm
;;
;; Test quotation consumption in EqualLink
;;
;; The main clause of `gl`
;;
;;       (QuoteLink
;;         (LambdaLink
;;           (UnquoteLink
;;             (VariableNode "$vardecl"))
;;           (UnquoteLink
;;             (VariableNode "$body"))))
;;
;; matches the grounding `top`
;;
;;   (Lambda
;;     (Variable "$X")
;;     (Variable "$X"))
;;
;; However, if quotation consumption is not subsequently properly
;; handled, the virtual clause
;;
;;       (top?
;;         (QuoteLink
;;           (LambdaLink
;;             (UnquoteLink
;;               (VariableNode "$vardecl"))
;;             (UnquoteLink
;;               (VariableNode "$body")))))))
;;
;; produces
;;
;; (EqualLink
;;   (QuoteLink
;;     (LambdaLink
;;       (UnquoteLink
;;         (Variable "$X"))
;;       (UnquoteLink
;;         (Variable "$X"))))
;;   (Lambda
;;     (Variable "$X")
;;     (Variable "$X")))
;;
;; instead of
;;
;; (EqualLink
;;   (Lambda
;;     (Variable "$X")
;;     (Variable "$X"))
;;   (Lambda
;;     (Variable "$X")
;;     (Variable "$X")))
;;
;; thus falsely discarding the match.

(use-modules (opencog) (opencog exec))

(define top
  (Lambda
    (Variable "$X")
    (Variable "$X"))
)

(define (top? x)
  (Equal
    x
    top)
)

(define gl
  (GetLink
    (VariableList
      (VariableNode "$vardecl")
      (VariableNode "$body"))
    (AndLink
      (QuoteLink
        (LambdaLink
          (UnquoteLink
            (VariableNode "$vardecl"))
          (UnquoteLink
            (VariableNode "$body"))))
      (top?
        (QuoteLink
          (LambdaLink
            (UnquoteLink
              (VariableNode "$vardecl"))
            (UnquoteLink
              (VariableNode "$body")))))))
)

; (cog-execute! gl)

(define expect
  (SetLink
    (ListLink
      (VariableNode "$X")
      (VariableNode "$X")))
)
