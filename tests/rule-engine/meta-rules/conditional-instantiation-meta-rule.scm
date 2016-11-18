;; =======================================================================
;; Crisp Conditional Instantiation Meta Rule
;;
;; ImplicationScopeLink
;;    V
;;    P
;;    Q
;; |-
;;    T
;;    P
;;    |-
;;    Q[V->T]
;;
;; where V is a variable or a list of variables, P is a condition, Q
;; is the implicand, T is an atom (or a list of atoms) satisfying the
;; V type constraints, and such that P[V->T], P where V has been
;; subtituted by T has a meaningful TV, and Q[V->T] is Q where V has
;; been substituted by T.
;; -----------------------------------------------------------------------

(use-modules (srfi srfi-1))

(use-modules (opencog exec))
(use-modules (opencog logger))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implication full instantiation rule ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define conditional-full-instantiation-meta-variables
  (VariableList
     (TypedVariable
        (Variable "$TyVs")
        (TypeChoice
           (Type "TypedVariableLink")
           (Type "VariableList")))
     (Variable "$P")
     (Variable "$Q")))

(define conditional-full-instantiation-meta-body
  (LocalQuote (ImplicationScope
     (Variable "$TyVs")
     (Variable "$P")
     (Variable "$Q"))))

;; Here only the implicant is considered as premise. The variable(s)
;; should as well so the backward chainer can consider them as
;; targets, however that implies to lay them out explicitly, so for
;; sake of simplicity they are ignored for now.
(define conditional-full-instantiation-meta-rewrite
  (Quote (Bind
     (Unquote (Variable "$TyVs"))
     (Unquote (Variable "$P"))
     (ExecutionOutput
        (GroundedSchema "scm: conditional-full-instantiation-formula")
        (ListLink
           (Unquote conditional-full-instantiation-meta-body)
           (Unquote (Variable "$P"))
           (Unquote (Variable "$Q")))))))

;; Bind
;;   VariableList
;;     TypedVariable
;;       Variable "$TyVs"
;;       TypeChoice
;;         Type "TypedVariableLink"
;;         Type "VariableLink"
;;     Variable "$P"
;;     Variable "$Q"
;;   LocalQuote
;;     ImplicationScope
;;       Variable "$TyVs"
;;       Variable "$P"
;;       Variable "$Q"
;;   LocalQuote
;;     Bind
;;       Variable "$TyVs"
;;       Variable "$P"
;;       ExecutionOutput
;;         GroundedSchema "scm: conditional-full-instantiation-formula"
;;         List
;;           LocalQuote
;;             ImplicationScope
;;               Variable "$TyVs"
;;               Variable "$P"
;;               Variable "$Q"
;;           Variable "$P"
;;           Variable "$Q"
(define conditional-full-instantiation-meta-rule
  (BindLink
     conditional-full-instantiation-meta-variables
     conditional-full-instantiation-meta-body
     conditional-full-instantiation-meta-rewrite))

;; And fuzzy eval. Normally this would be handled by a rule but to
;; simplify the test when hardcore it here
(define (and-fuzzy-eval a)
  ;; TODO
)

;; Set (stv 1 1) on Q is Impl and P strength are both above 0.5 and
;; their confidence is non null.
(define (conditional-full-instantiation-formula Impl P Q)
  (let* (
         (Impl-s (cog-stv-strength Impl))
         (Impl-c (cog-stv-confidence Impl))
         (P-s (cog-stv-strength P))
         (P-c (cog-stv-confidence P))
         (good-enough (and (> Impl-s 0.5) (> Impl-c 0) (> P-s 0.5) (> P-c 0))))
    (if (good-enough)
        (cog-merge-hi-conf-tv! Q (stv 1 1))
        (cog-undefined-handle))))

;; Name the meta rule
(define conditional-full-instantiation-meta-rule-name
  (DefinedSchemaNode "conditional-full-instantiation-meta-rule"))
(DefineLink conditional-full-instantiation-meta-rule-name
  conditional-full-instantiation-meta-rule)
