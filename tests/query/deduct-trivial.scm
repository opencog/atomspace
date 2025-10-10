;;
;; deduct-trivial.scm
;;
;; Trivial example of deduction.
;;
;; Part of the "Einstein puzzle" demo.
;;

;; The Englishman lives in the red house.
(EvaluationLink
	(PredicateNode "Nationality")
	(ListLink
		(FeatureNode "person1") ; AvatarNode
		(ConceptNode "British")
	)
)

(EvaluationLink
	(PredicateNode "LivesIn")
	(ListLink
		(FeatureNode "person1") ; AvatarNode
		(ConceptNode "red_house")
	)
)

;; The person who lives in the red house keeps fish.
(EvaluationLink
	(PredicateNode "LivesIn")
	(ListLink
		(FeatureNode "person2")
		(ConceptNode "red_house")
	)
)

(EvaluationLink
	(PredicateNode "KeepsPet")
	(ListLink
		(FeatureNode "person2")
		(ConceptNode "fish")
	)
)



