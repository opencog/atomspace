;
; true-container.scm
; TrueJoinUTest
; make sure constraints on the top type works.

(use-modules (opencog) (opencog exec))

; Data
(Member (Concept "A") (Concept "S"))
(Evaluation (Predicate "P") (List (Concept "A")))
(Similarity (Concept "foo") (Concept "bar"))

(define min-top
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(TypeChoice
			(Type 'EvaluationLink)
			(Type 'SimilarityLink))))

(define max-top
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(TypeChoice
			(Type 'EvaluationLink)
			(Type 'SimilarityLink))))
