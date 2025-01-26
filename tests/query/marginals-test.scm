;
; marginals-test.scm
;
; Unit test for storing results and marginals with the query itself.
; This is the future replacement for the old matrix API. Much cleaner.


(use-modules (opencog) (opencog exec))

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

