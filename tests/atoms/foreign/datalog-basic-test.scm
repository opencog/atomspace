;
; datalog-basic-test.scm - Store DataLog expressions.
; Make sure that the code doesn't crash.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "datalog-basic-test")
(test-begin tname)

(define ls (DatalogAst "likes(john, mary)."))
(define le (DatalogAst (EvaluationLink
  (PredicateNode "likes")
  (ListLink (ConceptNode "john") (ConceptNode "mary")))))

(test-assert "basic eval link" (equal? ls le))

(test-end tname)

(opencog-test-end)
