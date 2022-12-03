;
; Port of UnifyUTest.cxxtest
; Unfinished, under construction

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

; General setup
(Inheritance (Concept "A") (Concept "B"))

(opencog-test-runner)

; --------------------------------------
; skip because its trivial: UnifyUTest::test_unify_basic_1
; skip because its trivial: UnifyUTest::test_unify_basic_2
; skip because its trivial: UnifyUTest::test_unify_basic_3
; --------------------------------------
(test-begin "UnifyUTest::test_unify_basic_4")
(define tub4
	(cog-execute!
		(Get
			(VariableList (Variable "$X") (Variable "$Y"))
			(Identical
				(Inheritance (Variable "$X") (Concept "B"))
				(Inheritance (Concept "A") (Variable "$Y"))))))

(format #t "Got ~A\n" tub4)
(test-assert "UnifyUTest::test_unify_basic_4"
	(equal? tub4
		(Set (List (Concept "A") (Concept "B")))))

(test-end "UnifyUTest::test_unify_basic_4")

; --------------------------------------
(test-begin "UnifyUTest::test_unify_basic_5")
(define tub5
	(cog-execute!
		(Get
			(Variable "$X")
			(Identical
				(Inheritance (Variable "$X") (Variable "$Y"))
				(Inheritance (Concept "A") (Variable "$Y"))))))

(format #t "Got ~A\n" tub5)
(test-assert "UnifyUTest::test_unify_basic_5"
	(equal? tub5
		(Set (Concept "A"))))

(test-end "UnifyUTest::test_unify_basic_5")

; --------------------------------------
; skip because its trivial: UnifyUTest::test_unify_basic_6
; skip because its trivial: UnifyUTest::test_unify_basic_7
; --------------------------------------
(test-begin "UnifyUTest::test_unify_basic_8")
(define tub8
	(cog-execute!
		(Get
			(VariableList (Variable "$X") (Variable "$Y"))
			(Identical
				(Inheritance (Variable "$X") (Variable "$Y"))
				(Inheritance (Concept "A") (Variable "$Z"))))))

(format #t "Got ~A\n" tub8)
(test-assert "UnifyUTest::test_unify_basic_8"
	(equal? tub8
		(Set (List (Concept "A") (Variable "$Z")))))

(test-end "UnifyUTest::test_unify_basic_8")

; --------------------------------------
; Stub out. Throws error
;  The variable (VariableNode "$Y") does not appear (unquoted) in any clause!
;;;(test-begin "UnifyUTest::test_unify_undeclared_var_1")
;;;(define tuuv1
;;;	(cog-execute!
;;;		(Get
;;;			(Variable "$Y")
;;;			(Identical
;;;				(Variable "$X")
;;;				(Concept "A")))))
;;;
;;;(format #t "Got ~A\n" tuuv1)
;;;(test-assert "UnifyUTest::test_unify_undeclared_var_1"
;;;	(equal? tuuv1 (Set)))
;;;
;;;(test-end "UnifyUTest::test_unify_undeclared_var_1")

(opencog-test-end)
