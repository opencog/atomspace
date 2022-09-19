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

; Shouldn't throw.
(define ls2 (DatalogAst "likes(john, 'M foo, (()), barf, mary')."))
(define le2 (DatalogAst (EvaluationLink (PredicateNode "likes")
  (ListLink (ConceptNode "john") (ConceptNode "'M foo, (()), barf, mary'")))))

(test-assert "quote eval link" (equal? ls2 le2))


(test-end tname)

(opencog-test-end)
