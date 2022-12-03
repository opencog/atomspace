;
; Port of UnifyUTest.cxxtest
; Unfinished, under construction

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

; General setup
(Inheritance (Concept "A") (Concept "B"))

(opencog-test-runner)

(test-begin "UnifyUTest::test_unify_4")
(define ru4
	(cog-execute!
		(Get
			(VariableList (Variable "$X") (Variable "$Y"))
			(Identical
				(Inheritance (Variable "$X") (Concept "B"))
				(Inheritance (Concept "A") (Variable "$Y"))))))

(format #t "Got ~A\n" ru4)
(test-assert "UnifyUTest::test_unify_4"
	(equal? ru4
		(Set (List (Concept "A") (Concept "B")))))

(test-end "UnifyUTest::test_unify_4")

(opencog-test-end)
