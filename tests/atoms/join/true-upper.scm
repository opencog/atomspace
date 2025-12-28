;
; true-upper.scm
; TrueJoinUTest
; Validate that UpperSet works correctly.

(use-modules (opencog))

(Evaluation (Predicate "ontology")
	(List
		(Concept "class")
		(Member (Concept "crow") (Concept "bird"))))

(define min-join
	(MinimalJoin
		(Concept "crow")
		(Concept "bird")))

(define max-join
	(MaximalJoin
		(Concept "crow")
		(Concept "bird")))

(define upper-set
	(UpperSet
		(Concept "crow")
		(Concept "bird")))
