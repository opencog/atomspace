; =============================================================================
; Fuzzy conjunction introduction rule
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

(use-modules (opencog rule-engine))
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

;; Generate a fuzzy conjunction introduction rule for an n-ary
;; conjunction
(define (gen-fuzzy-conjunction-introduction-rule nary)
  (let* ((variables (gen-variables "$X" nary))
         (EvaluationT (Type "EvaluationLink"))
         (InheritanceT (Type "InheritanceLink"))
         (type (TypeChoice EvaluationT InheritanceT))
         (gen-typed-variable (lambda (x) (TypedVariable x type)))
         (vardecl (VariableList (map gen-typed-variable variables)))
         (pattern (And variables))
         (rewrite (ExecutionOutput
                    (GroundedSchema "scm: fuzzy-conjunction-introduction-formula")
                    ;; We wrap the variables in Set because the order
                    ;; doesn't matter and that way alpha-conversion
                    ;; works better.
                    (List (And variables) (Set variables)))))
    (Bind
      vardecl
      pattern
      rewrite)))

;; Return true if all elements of the list are unique
(define (is-set l)
  (cond ((null? l) #t)
        ((member (car l) (cdr l)) #f)
        (else (is-set (cdr l)))))

;; Check that they all are different, and have positive confidences
(define (fuzzy-conjunction-introduction-precondition S)
  (bool->tv (is-confident-enough-set (cog-outgoing-set S))))

(define (is-confident-enough-set andees)
  (let* ((confident-enough (lambda (A) (> (cog-stv-confidence A) 0))))
    (and (is-set andees)
         (every confident-enough andees))))

(define (fuzzy-conjunction-introduction-formula A S)
  (let* ((andees (cog-outgoing-set S))
         (min-s-atom (min-element-by-key andees cog-stv-strength))
         (min-c-atom (min-element-by-key andees cog-stv-confidence))
         (min-s (cog-stv-strength min-s-atom))
         (min-c (cog-stv-confidence min-c-atom)))
    (if (is-confident-enough-set andees)       ; only introduce meaningful
                                               ; conjunction of unique andees
        (cog-set-tv! A (stv min-s min-c)))))

;; Name the rules
(define fuzzy-conjunction-introduction-2ary-rule
  (gen-fuzzy-conjunction-introduction-rule 2))
(define fuzzy-conjunction-introduction-2ary-rule-name
  (DefinedSchema "fuzzy-conjunction-introduction-2ary-rule"))
(DefineLink
  fuzzy-conjunction-introduction-2ary-rule-name
  fuzzy-conjunction-introduction-2ary-rule)
