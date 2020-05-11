;
; gpn-join.scm
; GPNJoinUTest
; Make sure grounded predicate nodes work.

(use-modules (opencog) (opencog exec))

(Evaluation (Predicate "cherry pie") (List (Concept "A")))
(Evaluation (Predicate "hello daddy") (List (Concept "B")))
(Evaluation (Predicate "hello mum") (List (Concept "C")))
(Evaluation (Predicate "ch ch ch cherry bomb") (List (Concept "Runaways")))
(Evaluation (Predicate "cutie pie") (List (Concept "E")))
(Evaluation (Predicate "cake") (List (Concept "F")))
(Evaluation (Predicate "spinach") (List (Concept "G")))
(Evaluation (Predicate "sinister shoes") (List (Concept "Zappa")))

(define (like-pie ATOM)
	(define label (cog-name ATOM))
	(format #t "I was told ~A" ATOM)
	(if (string-contains label "pie") (stv 1 1) (stv 0 1)))

(define min-gpn
	(MinimalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "P")))))

(define max-gpn
	(MaximalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "P")))))

(define min-gpn-rep
	(MinimalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "P")))))

(define max-gpn-rep
	(MaximalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "P")))))
