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

(load-from-path "tests/rule-engine/rules/instantiation.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implication full instantiation rule ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define crisp-conditional-full-instantiation-meta-variables
  (VariableList
     (TypedVariable
        (Variable "$TyVs")
        (TypeChoice
           (Type "TypedVariableLink")
           (Type "VariableList")))
     (Variable "$P")
     (Variable "$Q")))

(define crisp-conditional-full-instantiation-meta-body
  (LocalQuote (ImplicationScope
     (Variable "$TyVs")
     (Variable "$P")
     (Variable "$Q"))))

;; Here only the implicant is considered as premise. The variable(s)
;; should as well so the backward chainer can consider them as
;; targets, however that implies to lay them out explicitly, so for
;; sake of simplicity they are ignored for now.
(define crisp-conditional-full-instantiation-meta-rewrite
  (LocalQuote (Bind
     (Variable "$TyVs")
     (Variable "$P")
     (ExecutionOutput
        (GroundedSchema "scm: crisp-conditional-full-instantiation-formula")
        (ListLink
           crisp-conditional-full-instantiation-meta-body
           (Variable "$P")
           (Variable "$Q"))))))

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
;;         GroundedSchema "scm: crisp-conditional-full-instantiation-formula"
;;         List
;;           LocalQuote
;;             ImplicationScope
;;               Variable "$TyVs"
;;               Variable "$P"
;;               Variable "$Q"
;;           Variable "$P"
;;           Variable "$Q"
(define crisp-conditional-full-instantiation-meta-rule
  (BindLink
     conditional-full-instantiation-meta-variables
     conditional-full-instantiation-meta-body
     conditional-full-instantiation-meta-rewrite))

;; Set (stv 1 1) on Q is Impl and P strength are both above 0.5 and
;; their confidence is non null.
(define (crisp-conditional-full-instantiation-formula Impl P Q)
  (let* (
         (Impl-s (cog-stv-strength Impl))
         (Impl-c (cog-stv-confidence Impl))
         (P-s (cog-stv-confidence P))
         (P-c (cog-stv-confidence P))
         (strong-enough (and (> Impl-s 0.5) (> Impl-c 0) (> P-s 0.5) (> P-c 0)))
         (Q-s (if strong-enough 1 0))
         (Q-c (if strong-enough 1 0)))
    (cog-set-tv! Q (stv Q-s Q-c))))

;; Name the meta rule
(define crisp-implication-full-instantiation-meta-rule-name
  (DefinedSchemaNode "crisp-implication-full-instantiation-meta-rule"))
(DefineLink crisp-implication-full-instantiation-meta-rule-name
  crisp-implication-full-instantiation-meta-rule)
