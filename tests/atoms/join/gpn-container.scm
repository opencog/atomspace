;
; gpn-container.scm
; GPNJoinUTest
; Make sure named containers work.

(use-modules (opencog))

(Evaluation (Predicate "cherry pie") (List (Concept "A")))
(Evaluation (Predicate "hello daddy") (List (Concept "B")))
(Evaluation (Predicate "hello mum") (List (Concept "C")))
(Evaluation (Predicate "ch ch ch cherry bomb") (List (Concept "Runaways")))
(Evaluation (Predicate "cutie pie") (List (Concept "E")))
(Evaluation (Predicate "cake") (List (Concept "F")))
(Evaluation (Predicate "spinach") (List (Concept "G")))
(Evaluation (Predicate "sinister shoes") (List (Concept "Zappa")))

(define (min-like-pie ATOM)
	(define label (cog-name ATOM))
	(format #t "I was minimally told ~A" ATOM)
	(if (string-contains label "pie") #t #f))

(define (max-like-pie ATOM)
	(define pred (cog-value-ref ATOM 0))
	(define label (cog-name pred))
	(format #t "I was maximally told ~A" ATOM)
	(if (string-contains label "pie") #t #f))

(define (like-both-pie PRED EVAL)
	(define pred (cog-value-ref EVAL 0))
	(define label (cog-name pred))
	(format #t "I was told both ~A and ~A" PRED EVAL)
	(if (not (equal? PRED pred))
		(throw 'test-failure "like-both-pie" "You blew it!"))
	(if (string-contains label "pie") #t #f))

(define (like-triple-pie PRED ARG EVAL)
	(define pred (cog-value-ref EVAL 0))
	(define lst (cog-value-ref EVAL 1))
	(define arg (cog-value-ref lst 0))
	(define label (cog-name pred))
	(format #t "--------------------------\n")
	(format #t "I tripled ~A and ~A and ~A" PRED ARG EVAL)
	(if (not (equal? PRED pred))
		(throw 'test-failure "like-triple-pie" "Not what I wanted!"))
	(if (not (equal? ARG arg))
		(throw 'test-failure "like-triple-pie" "Bad bad bad!"))
	(if (string-contains label "pie")
		(format #t "Yes its pie!\n\n")
		(format #t "No its not!\n\n"))
	(if (string-contains label "pie") #t #f))

; Exclude internal marker atoms from matching
(define marker (Predicate "*-IsKeyFlag-*"))

(define min-gpn
	(MinimalJoin
		(VariableList
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Evaluation (GroundedPredicate "scm:min-like-pie")
			(List (Variable "$top")))))

(define max-gpn
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Evaluation (GroundedPredicate "scm:max-like-pie")
			(List (Variable "$top")))))

(define both-gpn
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Evaluation (GroundedPredicate "scm:like-both-pie")
			(List (Variable "P") (Variable "$top")))))

(define triple-gpn
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "A") (Type 'ConceptNode))
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Present (Evaluation (Variable "P") (List (Variable "A"))))
		(Evaluation (GroundedPredicate "scm:like-triple-pie")
			(List (Variable "P") (Variable "A") (Variable "$top")))))

(define min-gpn-rep
	(MinimalJoin
		(VariableList
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:min-like-pie")
			(List (Variable "$top")))))

(define max-gpn-rep
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:max-like-pie")
			(List (Variable "$top")))))

(define both-gpn-rep
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Evaluation (GroundedPredicate "scm:like-both-pie")
			(List (Variable "P") (Variable "$top")))))

(define triple-gpn-rep
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "A") (Type 'ConceptNode))
			(TypedVariable (Variable "P") (Type 'PredicateNode))
			(TypedVariable (Variable "$top") (Type 'JoinLink)))
		(Not (Equal (Variable "P") marker))
		(Present (Evaluation (Variable "P") (List (Variable "A"))))
		(Replacement (Variable "P") (Concept "I Like Pie!"))
		(Replacement (Variable "A") (Concept "Loser"))
		(Evaluation (GroundedPredicate "scm:like-triple-pie")
			(List (Variable "P") (Variable "A") (Variable "$top")))))
