;; =======================================================================
;; Crisp Conditional Instantiation Meta Rule
;;
;; ImplicationScopeLink
;;    V
;;    P
;;    Q
;; |-
;;    T
;;    P[V->T].tv.s * P[V->T].tv.c > 0
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

(define conditional-full-instantiation-variables
  (VariableList
     (TypedVariable
        (Variable "$TyVs")
        (TypeChoice
           (Type "TypedVariableLink")
           (Type "VariableList")))
     (Variable "$P")
     (Variable "$Q")))

(define conditional-full-instantiation-body
  (Quote (ImplicationScope
     (Unquote (Variable "$TyVs"))
     (Unquote (Variable "$P"))
     (Unquote (Variable "$Q")))))

;; TODO

;; Here only the implicant is considered as premise, while the
;; variable(s) should as well, but that implies to lay them out
;; explicitly so for sake of simplicity it is ignored for now.
(define conditional-full-instantiation-rewrite
  (ExecutionOutput
     (GroundedSchema "scm: conditional-full-instantiation-meta-formula")
     (List
        (Quote (Bind
           (Unquote (Variable "$TyVs"))
           (Unquote (Variable "$P"))
           (Unquote (ExecutionOutput
              (GroundedSchema "scm: conditional-full-instantiation-formula")
              (ListLink
                 (Variable "$P")
                 (Variable "$Q")))))))))

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
;;           Variable "$P"
;;           Variable "$Q"
(define conditional-full-instantiation-meta-rule
  (BindLink
     conditional-full-instantiation-variables
     conditional-full-instantiation-body
     conditional-full-instantiation-rewrite))

(define (conditional-full-instantiation-formula P Q)
  (cog-set-tv! Q (cog-tv
  (let* (
         (Impl-outgoings (cog-outgoing-set Impl))
         (TyVs (car Impl-outgoings))
         (P (cadr Impl-outgoings))
         (Q (caddr Impl-outgoings))
         (terms (select-conditioned-substitution-terms TyVs P)))
    (if (equal? terms (cog-undefined-handle))
        terms
        ;; Substitute the variables by the terms in the body
        (let* ((put (PutLink (LambdaLink TyVs Q) terms))
               (inst (cog-execute! put)))
          ;; Remove the PutLink to not pollute the atomspace
          (extract-hypergraph put)
          (cog-set-tv! inst (cog-tv Impl))))))

;; Name the meta rule
(define implication-full-instantiation-rule-name
  (DefinedSchemaNode "implication-full-instantiation-rule"))
(DefineLink implication-full-instantiation-rule-name
  implication-full-instantiation-rule)
