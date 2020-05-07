;
; blue-sky.scm
; JoinLink unit test ... more complicated.

(use-modules (opencog) (opencog exec))

(Context
   (Concept "moon")
   (Similarity
       (Evaluation (Predicate "is blue") (List (Concept "sky")))
       (Concept "unlikely")))

(Context
   (Concept "moon")
   (Similarity
       (Evaluation (Predicate "is blue") (List (Concept "sea")))
       (Concept "unlikely")))

(define max-blue-sky
	(MaximalJoin
		(TypedVariable (Variable "X")
			(Signature
				(Evaluation (Predicate "is blue") (List (Concept "sky")))))
		(Present (Variable "X"))))

(define min-blue-sky
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

(define max-blue-out
	(MaximalJoin
		(TypedVariable (Variable "X")
			(Signature
				(Evaluation (Predicate "is blue") (List (Type 'ConceptNode)))))
		(Present (Variable "X"))))

(define min-blue-out
	(MinimalJoin
		(TypedVariable (Variable "X")
			(Signature
				(Evaluation (Predicate "is blue") (List (Type 'ConceptNode)))))
		(Present (Variable "X"))))

