;; =======================================================================
;; Conditional Instantiation Meta Rule
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
  (let* ((implication (Quote (ImplicationScope
                         (Unquote (Variable "$TyVs"))
                         (Unquote (Variable "$P"))
                         (Unquote (Variable "$Q")))))
         (precondition (Evaluation
                         (GroundedPredicate "scm: true-enough")
                         implication)))
  (And
    implication
    precondition)))

;; Here only the implicant is considered as premise. The variable(s)
;; should as well so the backward chainer can consider them as
;; targets, however that implies to lay them out explicitly, so for
;; sake of simplicity they are ignored for now.
(define conditional-full-instantiation-meta-rewrite
  (let* ((TyVs (Variable "$TyVs"))
         (P (Variable "$P"))
         (Q (Variable "$Q"))
         (implication (Quote (ImplicationScope
                         (Unquote TyVs)
                         (Unquote P)
                         (Unquote Q)))))
    (Quote (Bind
      (Unquote TyVs)
      (And
        (Unquote (LocalQuote (LocalQuote P)))
        (Evaluation (GroundedPredicate "scm: true-enough") (Unquote P)))
      (ExecutionOutput
        (GroundedSchema "scm: conditional-full-instantiation-formula")
        (Unquote
          (ListLink
            Q
            implication
            P)))))))

(define conditional-full-instantiation-meta-rule
  (BindLink
     conditional-full-instantiation-meta-variables
     conditional-full-instantiation-meta-body
     conditional-full-instantiation-meta-rewrite))

;; Return the TV of the evaluation of `an` assuming that it is an
;; AndLink, that is return the TV which is the the min over the
;; strengths and confidences of its outgoings. Normally this would be
;; handled by a rule but to simplify the unit test it is hardcored
;; here.
(define (conjunction-fuzzy-eval an)
  (let* ((outg (cog-outgoing-set an))
         (min-s-atom (min-element-by-key outg cog-stv-strength))
         (min-c-atom (min-element-by-key outg cog-stv-confidence))
         (min-s (cog-stv-strength min-s-atom))
         (min-c (cog-stv-confidence min-s-atom)))
    (stv min-s min-c)))

(define (true-enough-bool a)
  (let ((s (cog-stv-strength a)) (c (cog-stv-confidence a)))
    (and (> s 0.5) (> c 0.5))))

(define (true-enough a)
  (bool->tv (true-enough-bool a)))

;; Set (stv 1 1) on Q is Impl and P strength are both above 0.5 and
;; their confidence is non null.
(define (conditional-full-instantiation-formula Q Impl P)
  ;; Evaluate Q
  (if (and (true-enough-bool Impl) (true-enough-bool P))
      (cog-set-tv! Q (stv 1 1))))

;; Name the meta rule
(define conditional-full-instantiation-meta-rule-name
  (DefinedSchemaNode "conditional-full-instantiation-meta-rule"))
(DefineLink conditional-full-instantiation-meta-rule-name
  conditional-full-instantiation-meta-rule)
