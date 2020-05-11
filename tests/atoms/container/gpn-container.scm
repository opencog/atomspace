;
; gpn-container.scm
; GPNJoinUTest
; Make sure named containers work.

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
	(define pred (cog-outgoing-atom ATOM 0))
	(define label (cog-name pred))
	(format #t "I was told ~A" ATOM)
	(if (string-contains label "pie") (stv 1 1) (stv 0 1)))

(define min-gpn
	(MinimalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(TypedVariable (Variable "$top") (Type 'JoinNode))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "$top")))))

(define max-gpn
	(MaximalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(TypedVariable (Variable "$top") (Type 'JoinNode))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "$top")))))

(define min-gpn-rep
	(MinimalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(TypedVariable (Variable "$top") (Type 'JoinNode))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "$top")))))

(define max-gpn-rep
	(MaximalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(TypedVariable (Variable "$top") (Type 'JoinNode))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:like-pie") (List (Variable "$top")))))
