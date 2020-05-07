;
; blue-sky.scm
; JoinLink unit test ... more complicated.

(use-modules (opencog) (opencog exec))

(Evaluation (Predicate "is blue") (List (Concept "sky")))

(Context 
   (Concept "moon")
   (Similarity 
       (Evaluation (Predicate "is blue") (List (Concept "sky")))
       (Concept "unlikely")))

(define max-blue
	(MaximalJoin
		(TypedVariable (Variable "X")
			(Signature 
				(Evaluation (Predicate "is blue") (List (Concept "sky")))))
		(Present (Variable "X"))))

(define min-blue
	(MinimalJoin
		(TypedVariable (Variable "X")
			(Signature 
				(Evaluation (Predicate "is blue") (List (Concept "sky")))))
		(Present (Variable "X"))))

(define max-replace
	(MaximalJoin
		(TypedVariable (Variable "X")
			(Signature 
				(Evaluation (Predicate "is blue") (List (Concept "sky")))))
		(Present (Variable "X"))
		(Replacement (Variable "X") (Concept "green cheese"))))

(define min-replace
	(MinimalJoin
		(TypedVariable (Variable "X")
			(Signature 
				(Evaluation (Predicate "is blue") (List (Concept "sky")))))
		(Present (Variable "X"))
		(Replacement (Variable "X") (Concept "green cheese"))))
