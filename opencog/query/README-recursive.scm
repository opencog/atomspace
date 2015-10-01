;;
;; Some half-finished working notes on recursive patterns.
;; The goal is not to define recursive functions, but instead
;; to define recursive patterns that the pattern matcher can
;; solve.
;;
;; This stuff is broken! Beware! it might not even make sense!!


;; This is completely wrong, because it mixes TV's and atoms!
;; To make this work correctly, we need to invent a link that says
;; "if(tv) then atom", and convert to GroundedSchemaNodes...
(DefineLink
	(DefinedPredicateNode "factorial")
	(LambdaLink
		(VariableNode "$n")
		(SequentialAndLink
			;; If the first clause evaluates to true,
			;; then proceed to the next clause
			(GreaterThanLink (VariableNode "$n") (NumberNode 1))
			(TimesLink
				(VariableNode "$n")
				(PutLink
					(DefinedPredicateNode "factorial")
					(PlusLink (VariableNode "$n") (NumberNode -1)))))))


;; Hierarchical noun-phrase detector.
(DefineLink
	(DefinedSchemaNode "NP detector")
	(LambdaLink
		(VariableNode "$word")
		;; ChoiceLink means that either the first or the second pattern
		;; must be found in the atomspace.
		(ChoiceLink
			;; So either the below can be found in the atomspace, and
			;; the pattern matcher returns the actual word...
			(EvaluationLink
				(PredicateNode "Noun")
				(ListLink
					(VariableNode "$word")))
			;; Or, instead, we look for NP's.
			(EvaluationLink
				(PredicateNode "NP")
				(ListLink
					(VariableNode "$word")
					(DefinedSchemaNode "NP detector"))))))

;; Above pattern should find the below.
(EvaluationLink
	(PredicateNode "NP")
	(ListLink
		(WordNode "a")
		(EvaluationLink
			(PredicateNode "NP")
			(ListLink
				(WordNode "noun")
				(EvaluationLink
					(PredicateNode "Noun")
					(ListLink
						(WordNode "phrase")))))))
		
 

;; A flattened NP detector ...

(DefineLink
	(DefinedSchemaNode "flat NP detector")
	(LambdaLink
		(VariableNode "$word")
		(VariableNode "$np")
		;; ChoiceLink means that either the first or the second pattern
		;; must be found in the atomspace.
		(ChoiceLink
			;; So either the below can be found in the atomspace, and
			;; the pattern matcher returns the actual word...
			(EvaluationLink
				(PredicateNode "Noun")
				(ListLink
					(VariableNode "$word")))
			;; Or, instead, we look for WordPair's.
			(AndLink
				(EvaluationLink
					(PredicateNode "WordPair")
					(ListLink
						(VariableNode "$word")
						(VariableNode "$np")))
				(PutLink ;; This means: substitute the two variables
					(DefinedSchemaNode "flat NP detector")
					(ListLink
						(VariableNode "$word")
						(VariableNode "$np"))) ))))


; Above should find the below...

(EvaluationLink
	(PredicateNode "Determiner")
	(ListLink (WordNode "a")))

(EvaluationLink
	(PredicateNode "Adjective")
	(ListLink (WordNode "short")))

(EvaluationLink
	(PredicateNode "Noun")
	(ListLink (WordNode "phrase")))

(EvaluationLink
	(PredicateNode "WordPair")
	(ListLink (WordNode "short") (WordNode "phrase")))

(EvaluationLink
	(PredicateNode "WordPair")
	(ListLink (WordNode "a") (WordNode "short")))
