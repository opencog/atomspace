;
; true-upper.scm
; TrueJoinUTest
; Validate that UpperSet works correctly.

(use-modules (opencog) (opencog exec))

(Evaluation (Predicate "ontology")
	(List
		(Concept "class")
		(Member (Concept "crow") (Concept "bird"))))

(define min-join
	(MinimalJoin
		(Present (Concept "crow"))
		(Present (Concept "bird"))))

(define max-join
	(MaximalJoin
		(Present (Concept "crow"))
		(Present (Concept "bird"))))

(define upper-set
	(UpperSet
		(Present (Concept "crow"))
		(Present (Concept "bird"))))
