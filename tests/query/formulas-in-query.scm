;
; formulas-in-query.scm
;
; Make sure that formulas work in search patterns.
; This tests the second bug reported in opencog/atomspace#2650
;
(use-modules (opencog) (opencog exec))

(define tvkey (Predicate "*-TruthValueKey-*"))
(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

(define larg (List (Concept "node1") (Concept "name1")))
(define evna (Evaluation (Predicate "has_name") larg))
(define mema (Member evna (Concept "node2")))

; The answer we always expect to get:
(define ans (List (Concept "node1") (Concept "name1")))

; Get whack errors if we do not set the truth values.
(define mytv FloatValue)
(cog-set-value! (Concept "node1") tvkey (mytv 1 0))
(cog-set-value! (Concept "name1") tvkey (mytv 1 0))
(cog-set-value! (Concept "node2") tvkey (mytv 1 0))
(cog-set-value! larg tvkey (mytv 1 0))
(cog-set-value! evna tvkey (mytv 1 0))
(cog-set-value! mema tvkey (mytv 1 0))

; Make sure this passes, before we get fancy
(define q-basic (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1"))))
  (Variable "Y")))

; Same as above but with IdenticalLink
(define qi-basic (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1"))))
	(Variable "Y")))

(define fumbula
   (Lambda
      (VariableList (Variable "$X") (Variable "$Y"))
      (FloatColumn
         (Minus
            (Number 1)
            (Times
               (strength-of (Variable "$X"))
               (strength-of (Variable "$Y"))))
         (Times
            (confidence-of (Variable "$X"))
            (confidence-of (Variable "$Y"))))))


(define qe1 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(ExecutionOutput
			fumbula
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe1)

; Same as above, but with multiple components (equal link)
(define qe2 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(ExecutionOutput
			fumbula
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe2)

; Same as above, but with IdenticalLink
(define qe2i (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(ExecutionOutput
			fumbula
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe2i)

(define qe3 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(ExecutionOutput
			(DefinedSchema "reddish")
			(Variable "Y")))
  (Variable "Y")))

; Same as above, but with Identical
(define qe3i (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(ExecutionOutput
			(DefinedSchema "reddish")
			(Variable "Y")))
  (Variable "Y")))

; The definition needed for the above.
(DefineLink (DefinedSchema "reddish") fumbula)

; (cog-execute! qe3)

; Like above, but without the EqualLink
(define qe4 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(ExecutionOutput
			(DefinedSchema "reddish")
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe4)


; A simpler more direct version
(define qe5 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(GreaterThan
			(strength-of (Variable "Y")) (Number 0.5)))
  (Variable "Y")))

; (cog-execute! qe5)

; A convoluted version of above.
(define qe6 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(GreaterThan
			(strength-of
				(ExecutionOutput
					(DefinedSchema "reddish")
					(Variable "Y")))
			(Number 0.5)))
  (Variable "Y")))

; (cog-execute! qe6)

; Finally, verbose and convoluted
(define qe7 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(GreaterThan
			(strength-of
				(ExecutionOutput
					fumbula
					(Variable "Y")))
			(Number 0.5)))
  (Variable "Y")))

; (cog-execute! qe7)
