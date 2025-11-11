;;;
;;; filter-more-greedy-test.scm
;;;
;;; Complicated GlobNode matching in FilterLink
;;;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "filter-more-greedy-test")
(test-begin tname)

;;;
;;; Real-world test: Link Grammar sentence with adjacent bonds
;;;
;;; Sentence: "Heat causes expansion"
;;; Structure: Has Ss(Heat,causes) and Ou(causes,expansion) bonds
;;;            that are ADJACENT (zero bonds between them)

(define parse-bonds
	(LinkValue
	  (LinkValue
		 (LinkValue
			(Item "###LEFT-WALL###")
			(Item "Heat")
			(Item "causes")
			(Item "expansion")
			(Item "###RIGHT-WALL###"))
		 (LinkValue
			(Edge
			  (Bond "RW")
			  (List
				 (Item "###LEFT-WALL###")
				 (Item "###RIGHT-WALL###")))
			(Edge
			  (Bond "WV")
			  (List
				 (Item "###LEFT-WALL###")
				 (Item "causes")))
			(Edge
			  (Bond "Wd")
			  (List
				 (Item "###LEFT-WALL###")
				 (Item "Heat")))
			(Edge
			  (Bond "Ss")
			  (List
				 (Item "Heat")
				 (Item "causes")))
			(Edge
			  (Bond "Ou")
			  (List
				 (Item "causes")
				 (Item "expansion")))))))

(cog-set-value! (Anchor "sent") (Predicate "bonds") parse-bonds)

;;;
;;;
;;; Test 1: Baseline - Regular Glob should FAIL on adjacent bonds
;;;

(define pattern-regular-glob
	(Filter
		(Rule
			(VariableList
				(TypedVariable (Variable "$words") (Type 'LinkValue))
				(Glob "$b1")
				(TypedVariable (Variable "$subj") (Type 'Item))
				(TypedVariable (Variable "$verb") (Type 'Item))
				(Glob "$mid")	; Regular glob - requires 1+ matches
				(TypedVariable (Variable "$obj") (Type 'Item))
				(Glob "$b2"))

			(LinkSignature (Type 'LinkValue)
				(Variable "$words")
				(LinkSignature (Type 'LinkValue)
					(Glob "$b1")
					(Edge (Bond "Ss") (List (Variable "$subj") (Variable "$verb")))
					(Glob "$mid")	; This glob requires 1+ bonds between Ss and Ou
					(Edge (Bond "Ou") (List (Variable "$verb") (Variable "$obj")))
					(Glob "$b2")))

			(Edge
				(LinkSignature (Type 'Predicate) (Variable "$verb"))
				(List
					(LinkSignature (Type 'Concept) (Variable "$subj"))
					(LinkSignature (Type 'Concept) (Variable "$obj")))))

		(ValueOf (Anchor "sent") (Predicate "bonds"))))

(define result-regular (cog-execute! pattern-regular-glob))
(define empty-result (LinkValue))

(test-assert "glob-fail-on-adjacent-bonds"
	(equal? result-regular empty-result))

;;;
;;; Test 2: Regression
;;;

(define pattern-interval-glob
	(Filter
		(Rule
			(VariableList
				(TypedVariable (Variable "$words") (Type 'LinkValue))
				(Glob "$b1")
				(TypedVariable (Variable "$subj") (Type 'Item))
				(TypedVariable (Variable "$verb") (Type 'Item))
				(TypedVariable (Glob "$mid")	; IntervalLink allows 0+ matches
					(Interval (Number 0) (Number -1)))
				(TypedVariable (Variable "$obj") (Type 'Item))
				(TypedVariable (Glob "$b2")	; IntervalLink allows 0+ matches
					(Interval (Number 0) (Number -1))))

			(LinkSignature (Type 'LinkValue)
				(Variable "$words")
				(LinkSignature (Type 'LinkValue)
					(Glob "$b1")
					(Edge (Bond "Ss") (List (Variable "$subj") (Variable "$verb")))
					(Glob "$mid")	; This glob NOW allows 0 bonds between Ss and Ou
					(Edge (Bond "Ou") (List (Variable "$verb") (Variable "$obj")))
					(Glob "$b2")))

			(Edge
				(LinkSignature (Type 'Predicate) (Variable "$verb"))
				(List
					(LinkSignature (Type 'Concept) (Variable "$subj"))
					(LinkSignature (Type 'Concept) (Variable "$obj")))))

		(ValueOf (Anchor "sent") (Predicate "bonds"))))

(define result-interval (cog-execute! pattern-interval-glob))

(test-assert "interval-glob-matches-adjacent-bonds"
	(not (equal? result-interval empty-result)))

;;; Also verify the result contains the expected extraction
(define result-list (cog-value->list result-interval))

(test-assert "result-contains-extraction"
	(> (length result-list) 0))

(test-end tname)
