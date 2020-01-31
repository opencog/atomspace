(use-modules (opencog) (opencog exec))

;; Definitions
(define (sp x)
"
  Return (stv 0.55 0.55) and store it into its own call, that is

  Evaluation (stv 0.55 0.55)
    GroundedPredicate \"scm:sp\"
    x
"
  (let* ((tv (stv 0.55 0.55))
         (gp (GroundedPredicate "scm:sp"))
         (ev (Evaluation gp x)))
    (cog-set-tv! ev tv)
    tv))

(define GP (GroundedPredicate "scm:sp"))
(define A (Concept "A"))
(define B (Concept "B"))
(define E (Evaluation GP A))

(define X (Variable "$X"))
;; Facts

(Inheritance A B)

;; Query
;; Note that this is a strange query -- the variable X never appears
;; in the EvaluationLink, and so this splits up as two disjoint queries
;; having nothing to do with each-other.

(define query
  (BindLink
    (AndLink
      E
      (InheritanceLink X B))
    E))
