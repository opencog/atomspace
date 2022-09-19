;
; datalog-basic-test.scm - Store DataLog expressions.
; Make sure that the code doesn't crash.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "datalog-basic-test")
(test-begin tname)

;; -----------
(define ls (DatalogAst "likes(john, mary)."))
(define le (DatalogAst
	(EvaluationLink (PredicateNode "likes")
		(ListLink (ConceptNode "john") (ConceptNode "mary")))))

(test-assert "basic eval link" (equal? ls le))

;; -----------
; Shouldn't throw.
(define ls2 (DatalogAst "likes(john, 'M foo, (()), barf, mary')."))
(define le2 (DatalogAst (EvaluationLink (PredicateNode "likes")
	(ListLink (ConceptNode "john") (ConceptNode "'M foo, (()), barf, mary'")))))

(test-assert "quote eval link" (equal? ls2 le2))

;; -----------
; Sue is a girl if she is the daughter of Mary
(define ls3 (DatalogAst "girl(sue) :- daughter(sue,mary)."))
(define le3 (DatalogAst
	(Implication
		(EvaluationLink
			(PredicateNode "daughter")
			(ListLink (ConceptNode "sue") (ConceptNode "mary")))
		(EvaluationLink
			(PredicateNode "girl")
			(ListLink (ConceptNode "sue"))))))

(test-assert "implication link" (equal? ls3 le3))

;; -----------
(define ls4 (DatalogAst " likes('John', car(bmw)). "))
(define le4 (DatalogAst
	(EvaluationLink
		(Predicate "likes")
		(ListLink
			(Concept "'John'")
			(EvaluationLink
				(Predicate "car")
				(ListLink (Concept "bmw")))))))

(test-assert "nested facts" (equal? ls4 le4))

;; -----------

(test-end tname)

(opencog-test-end)
