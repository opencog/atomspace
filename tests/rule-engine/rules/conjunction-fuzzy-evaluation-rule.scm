; =============================================================================
; ConjunctionEvaluationRule
;
; A1
; ...
; An
; |-
; AndLink
;   A1
;   ...
;   An
;
; Where A1 to An are atoms with a fuzzy TV
; -----------------------------------------------------------------------------

(use-modules (srfi srfi-1))

;; Generate variable (Variable prefix + "-" + to_string(i))
(define (gen-variable prefix i)
  (Variable (string-append prefix "-" (number->string i))))

;; Generate a list of variables (Variable prefix + "-" + to_string(n))
(define (gen-variables prefix n)
  (if (= n 0)
      ;; Base case
      '()
      ;; Recursive case
      (append (gen-variables prefix (- n 1))
              (list (gen-variable prefix (- n 1))))))

;; Generate a conjunction fuzzy evaluation rule for an n-ary
;; conjunction
(define (gen-conjunction-fuzzy-evaluation-rule nary)
  (let* ((variables (gen-variables "$X" nary))
         (EvaluationT (Type "EvaluationLink"))
         (InheritanceT (Type "InheritanceLink"))
         (type (TypeChoice EvaluationT InheritanceT))
         (gen-typed-variable (lambda (x) (TypedVariable x type)))
         (vardecl (VariableList (map gen-typed-variable variables)))
         (pattern (And variables))
         (rewrite (ExecutionOutput
                    (GroundedSchema "scm: conjunction-fuzzy-evaluation-formula")
                    ;; We wrap the variables in Set because the order
                    ;; doesn't matter and that way alpha-conversion
                    ;; works better.
                    (List (Set variables) (And variables)))))
    (Bind
      vardecl
      pattern
      rewrite)))

(define (conjunction-fuzzy-evaluation-formula S A)
  (let* ((andees (cog-outgoing-set S))
         (min-s-atom (min-element-by-key andees cog-stv-strength))
         (min-c-atom (min-element-by-key andees cog-stv-confidence))
         (min-s (cog-stv-strength min-s-atom))
         (min-c (cog-stv-confidence min-c-atom)))
    (cog-set-tv! A (stv min-s min-c))))

;; Name the rules
;;
;; Lame enumeration, maybe scheme can do better?
(define conjunction-fuzzy-evaluation-1ary-rule-name
  (DefinedSchema "conjunction-fuzzy-evaluation-1ary-rule"))
(DefineLink
  conjunction-fuzzy-evaluation-1ary-rule-name
  (gen-conjunction-fuzzy-evaluation-rule 1))
(define conjunction-fuzzy-evaluation-2ary-rule-name
  (DefinedSchema "conjunction-fuzzy-evaluation-2ary-rule"))
(DefineLink
  conjunction-fuzzy-evaluation-2ary-rule-name
  (gen-conjunction-fuzzy-evaluation-rule 2))
(define conjunction-fuzzy-evaluation-3ary-rule-name
  (DefinedSchema "conjunction-fuzzy-evaluation-3ary-rule"))
(DefineLink
  conjunction-fuzzy-evaluation-3ary-rule-name
  (gen-conjunction-fuzzy-evaluation-rule 3))
(define conjunction-fuzzy-evaluation-4ary-rule-name
  (DefinedSchema "conjunction-fuzzy-evaluation-4ary-rule"))
(DefineLink
  conjunction-fuzzy-evaluation-4ary-rule-name
  (gen-conjunction-fuzzy-evaluation-rule 4))
(define conjunction-fuzzy-evaluation-5ary-rule-name
  (DefinedSchema "conjunction-fuzzy-evaluation-5ary-rule"))
(DefineLink
  conjunction-fuzzy-evaluation-5ary-rule-name
  (gen-conjunction-fuzzy-evaluation-rule 5))
