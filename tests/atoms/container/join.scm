;
; join.scm
; JoinLink unit test.
; See `join-content.scm` for the data that these search for.

(use-modules (opencog) (opencog exec))

; --------------------------------------
(define max-join
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))))

(define min-join
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))))

(define max-replace
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))
		(Replacement (Variable "X") (Concept "B"))))

(define min-replace
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))
		(Replacement (Variable "X") (Concept "B"))))

; --------------------------------------
(define implicit-max-join
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))))

(define implicit-min-join
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))))

(define implicit-max-replace
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Replacement (Variable "X") (Concept "B"))))

(define implicit-min-replace
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Replacement (Variable "X") (Concept "B"))))

; --------------------------------------

(define shallow-join
	(MaximalJoin
		(TypedVariable (Variable "X") (Type 'ConceptNode))))

; --------------------------------------
(define max-const
	(MaximalJoin (Present (Concept "A"))))

(define min-const
	(MinimalJoin (Present (Concept "A"))))

(define max-const-replace
	(MaximalJoin
		(Present (Concept "A"))
		(Replacement (Concept "A") (Concept "B"))))

(define min-const-replace
	(MinimalJoin
		(Present (Concept "A"))
		(Replacement (Concept "A") (Concept "B"))))
