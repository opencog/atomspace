;; Used by ExecutionOutputUTest::test_query_exec

(use-modules (opencog) (opencog exec))

;; Grounds

(ExecutionOutput
  (Schema "sc-1")
  (Concept "a"))

(ExecutionOutput
  (Schema "sc-2")
  (Concept "b"))

(ExecutionOutput
  (Schema "sc-3")
  (Concept "c"))

;; Query

(define exec-query
  (Get (ExecutionOutput (Variable "$schema") (Variable "$arg"))))

(define quote-exec-query
  (Get
    (Quote
      (ExecutionOutput
        (Unquote (Variable "$schema"))
        (Unquote (Variable "$arg"))))))
