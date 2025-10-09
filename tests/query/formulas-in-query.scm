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

; Set the truth value so that the GreaterThanLink doesn't fail.
(define mytv FloatValue)
(cog-set-value! larg tvkey (mytv 1 0))

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

#! ---
 ; These are the accept values
 (cog-set-value! (Concept "node1") tvkey (FloatValue 0.5 0.6))
 (cog-set-value! (Concept "name1") tvkey (FloatValue 0.5 0.6))
 (cog-execute! (ExecutionOutput fumbula
    (List (Concept "node1") (Concept "name1"))))

 ; Returns (FloatValue 0.75 0.36)

 ; The reject values
 (cog-set-value! (Concept "node1") tvkey (FloatValue 0.9 0.3))
 (cog-set-value! (Concept "name1") tvkey (FloatValue 0.9 0.3))
 ; return (FloatValue 0.19 0.09)

-- !#


; evol needs to be evaluatable ... and must evaluate to true,
(define evol
	(GreaterThan
		(ElementOf (Number 0)
			(ExecutionOutput fumbula (Variable "Y")))
		(Number 0.5)))

(define qe1 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		evol)
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
		evol)
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
		evol)
  (Variable "Y")))

; (cog-execute! qe2i)

(define evodef
	(GreaterThan
		(ElementOf (Number 0)
			(ExecutionOutput (DefinedSchema "reddish") (Variable "Y")))
	(Number 0.5)))

(define qe3 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		evodef)
  (Variable "Y")))

; Same as above, but with Identical
(define qe3i (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		evodef)
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
		evodef)
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
