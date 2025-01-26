;
; marginals-test.scm
;
; Unit test for storing results and marginals with the query itself.
; This is the future replacement for the old matrix API. Much cleaner.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "marginals-test")
(test-begin tname)

; Data to prime the pump.
(Edge (Predicate "foo") (List (Item "left") (Item "right")))
(Edge (Predicate "foo") (List (Item "lefter") (Item "right")))
(Edge (Predicate "foo") (List (Item "leftest") (Item "right")))
(Edge (Predicate "foo") (List (Item "lefty") (Item "loosey")))

(define m (Meet
	(Variable "$x")
	(Present
		(Edge (Predicate "foo") (List (Variable "$x")(Item "right"))))))

(format #t "Meet keys ~A\n" (cog-keys m))
(cog-execute! m)
(format #t "Meet grnd ~A\n" (cog-value m m))
(format #t "Meet vars ~A\n" (cog-value m (Variable "$x")))

(test-assert "meet keynum" (equal? 2 (length (cog-keys m))))
(test-assert "meet grnds" (equal? 3 (length (cog-value->list
	(cog-value m m )))))
(test-assert "meet vars" (equal? 3 (length (cog-value->list
	(cog-value m (Variable "$x"))))))

; ----------------------------------------------------------

(define q (Query
	(Variable "$x")
	(Present
		(Edge (Predicate "foo") (List (Variable "$x")(Item "right"))))
	(Link (Item "fumble") (Variable "$x"))))

(format #t "Query keys ~A\n" (cog-keys q))
(cog-execute! q)

(format #t "Query results ~A\n" (cog-value q q))
(format #t "Query vars ~A\n" (cog-value q (Variable "$x")))
(format #t "Query implicand ~A\n"
	(cog-value q (Link (Item "fumble") (Variable "$x"))))

(test-assert "query keynum" (equal? 3 (length (cog-keys q))))
(test-assert "query grnds" (equal? 3 (length (cog-value->list
	(cog-value q q)))))
(test-assert "query vars" (equal? 3 (length (cog-value->list
	(cog-value q (Variable "$x"))))))

; ----------------------------------------------------------

(define q2 (Query
	(Variable "$x")
	(Present
		(Edge (Predicate "foo") (List (Variable "$x")(Item "right"))))
	(Link (Item "fumble") (Variable "$x"))
	(Link (Concept "bramble") (Variable "$x"))
	(Link (Item "borat") (Variable "$x") (Item "culture"))))

(format #t "Query keys ~A\n" (cog-keys q2))
(cog-execute! q2)

(format #t "Query results ~A\n" (cog-value q2 q2))
(format #t "Query vars ~A\n" (cog-value q2 (Variable "$x")))
(format #t "Query implicand one ~A\n"
	(cog-value q2 (Link (Item "fumble") (Variable "$x"))))
(format #t "Query implicand two ~A\n"
	(cog-value q2 (Link (Concept "bramble") (Variable "$x"))))
(format #t "Query implicand three ~A\n"
	(cog-value q2 (Link (Item "borat") (Variable "$x") (Item "culture"))))

(test-assert "q2 keynum" (equal? 5 (length (cog-keys q2))))
(test-assert "q2 grnds" (equal? 3 (length (cog-value->list
	(cog-value q2 q2)))))
(test-assert "q2 vars" (equal? 3 (length (cog-value->list
	(cog-value q2 (Variable "$x"))))))
(test-assert "q2 impl one" (equal? 3 (length (cog-value->list
	(cog-value q2 (Link (Item "fumble") (Variable "$x")))))))
(test-assert "q2 impl two" (equal? 3 (length (cog-value->list
	(cog-value q2 (Link (Concept "bramble") (Variable "$x")))))))
(test-assert "q2 impl three" (equal? 3 (length (cog-value->list
	(cog-value q2 (Link (Item "borat") (Variable "$x") (Item "culture")))))))

; ----------------------------------------------------------

(define m2 (Meet
	(Present
		(Edge (Predicate "foo") (List (Variable "$x")(Item "right"))))))

(define m3 (Meet
	(TypedVariable (Variable "$x") (Type "Item"))
	(Present
		(Edge (Predicate "foo") (List (Variable "$x")(Item "right"))))))

(define m4 (Meet
	(VariableList (TypedVariable (Variable "$x") (Type "Item")))
	(Present
		(Edge (Predicate "foo") (List (Variable "$x")(Item "right"))))))

(define m5 (Meet
	(VariableList
		(Variable "$loo")
		(TypedVariable (Variable "$ro") (Type "Item")))
	(Present
		(Edge (Predicate "foo")
			(List (Variable "$loo")(Variable "$ro"))))))

(format #t "Meet5 keys ~A\n" (cog-keys m5))
(cog-execute! m5)

(format #t "M5 results ~A\n" (cog-value m5 m5))
(format #t "M5 left vars ~A\n" (cog-value m5 (Variable "$loo")))
(format #t "M5 right vars ~A\n" (cog-value m5 (Variable "$ro")))
(format #t "M5 typed right vars ~A\n"
	(cog-value m5 (TypedVariable (Variable "$ro") (Type "Item"))))

(test-end tname)
(opencog-test-end)
