;
; blue-sky.scm
; JoinLink unit test ... more complicated.

(use-modules (opencog) (opencog exec))

; test data
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

;; ----------------------
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

;; ----------------------
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

;; ----------------------
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

;; ----------------------
(define max-blue-rep
	(MaximalJoin
		(TypedVariable (Variable "X")
			(Signature
				(Evaluation (Predicate "is blue") (List (Type 'ConceptNode)))))
		(Replacement (Variable "X") (Concept "green cheese"))
		(Present (Variable "X"))))

(define min-blue-rep
	(MinimalJoin
		(TypedVariable (Variable "X")
			(Signature
				(Evaluation (Predicate "is blue") (List (Type 'ConceptNode)))))
		(Replacement (Variable "X") (Concept "green cheese"))
		(Present (Variable "X"))))

;; ----------------------
(define max-thing
	(MaximalJoin
		(TypedVariable (Variable "$thing") (Type 'ConceptNode))
		(Present
			(Evaluation (Predicate "is blue") (List (Variable "$thing"))))))

(define min-thing
	(MinimalJoin
		(TypedVariable (Variable "$thing") (Type 'ConceptNode))
		(Present
			(Evaluation (Predicate "is blue") (List (Variable "$thing"))))))

;; ----------------------
(define max-thing-rep
	(MaximalJoin
		(TypedVariable (Variable "$thing") (Type 'ConceptNode))
		(Replacement (Variable "$thing") (Concept "green cheese"))
		(Present
			(Evaluation (Predicate "is blue") (List (Variable "$thing"))))))

(define min-thing-rep
	(MinimalJoin
		(TypedVariable (Variable "$thing") (Type 'ConceptNode))
		(Replacement (Variable "$thing") (Concept "green cheese"))
		(Present
			(Evaluation (Predicate "is blue") (List (Variable "$thing"))))))
