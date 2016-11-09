;; =======================================================================
;; Conditional Instantiation Meta Rule
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

;;;;;;;;;;;;;;;;;;;;;;;
;; Helper definition ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define conditional-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableList")))
     (VariableNode "$P")
     (VariableNode "$Q")))

(define conditional-instantiation-body
  (Quote (ImplicationScopeLink
     (Unquote (VariableNode "$TyVs"))
     (Unquote (VariableNode "$P"))
     (Unquote (VariableNode "$Q")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implication full instantiation rule ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here only the implicant is considered as premise, while the
;; variable(s) should as well, but that implies to lay them out
;; explicitly so for sake of simplicity it is ignored for now.
(define conditional-instantiation-rewrite
  (Quote (Bind
     (Unquote (VariableNode "$TyVs"))
     (Unquote (VariableNode "$P"))
     (Unquote (ExecutionOutputLink
       (GroundedSchemaNode "scm: conditional-instantiation-formula")
       (ListLink
          (VariableNode "$P")
          (VariableNode "$Q")))))))

;; TODO

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
;;         GroundedSchema "scm: conditional-instantiation-formula"
;;         List
;;           Variable "$X"
;;           Variable "$P"
;;           Variable "$Q"
(define conditional-instantiation-meta-rule
  (BindLink
     implication-full-instantiation-variables
     implication-instantiation-body
     implication-full-instantiation-rewrite))

;; TODO
;;
;; This function
;;
;; 1. randomly selects a substitution term (or a tuple of substitution
;;    terms, if the ImplicationLink has multiple variables in scope)
;;    that meets the implication's condition (the implicant),
;;
;; 2. performs the substitution.
;;
;; 3. calculates its TV (just the TV on the ImplicationLink)
;;
;; If no substitution is possible it returns the undefined handle
(define (implication-full-instantiation-formula Impl)
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
