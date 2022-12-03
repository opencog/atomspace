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

; --------------------------------------
; Stub out. Throws error
;  The variable (VariableNode "$Y") does not appear (unquoted) in any clause!
; Even if we didn't get this error, there is now a conceptual issue.
; Unify reports a result: it says that (Variable "$X") and (Variable "$X")
; are the same, because they are constants, and constant variables are
; by definition alpha-equivalent.
; The problem here is that we have no way to even report this result.
; So port this test to an alternate form, below.
;;;(test-begin "UnifyUTest::test_unify_undeclared_var_2")
;;;(define tuuv2
;;;	(cog-execute!
;;;		(Get
;;;			(Variable "$Y")
;;;			(Identical
;;;				(Variable "$X")
;;;				(Variable "$Z")))))
;;;
;;;(format #t "Got ~A\n" tuuv2)
;;;(test-assert "UnifyUTest::test_unify_undeclared_var_2"
;;;	(equal? tuuv2 (Set)))
;;;
;;;(test-end "UnifyUTest::test_unify_undeclared_var_2")

(test-begin "UnifyUTest::test_unify_undeclared_var_2_alt")
(define tuuv2
	(cog-execute!
		(Bind
			(Variable "$X")
			(Identical
				(Variable "$X")
				(Variable "$Z"))
			(List (Quote (Variable "$X")) (Variable "$X")))))

(format #t "Got ~A\n" tuuv2)
(test-assert "UnifyUTest::test_unify_undeclared_var_2_alt"
	(equal? tuuv2
		(Set (List (Variable "$X") (Variable "$Z")))))

(test-end "UnifyUTest::test_unify_undeclared_var_2_alt")

(opencog-test-end)
