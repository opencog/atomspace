;
; unordered-jswiergo.scm
;
; Unordered link nesting tests, from Jacek Świergocki
; AKA issue #148
;
(use-modules (opencog) (opencog exec))

(define shared (Similarity (Concept "A")))
(define tree (List shared (List shared (Concept "B"))))

(define query1 (Similarity (Variable "$a")))
(define query2 (Variable "$a"))

(define (tree-query input-query)
	(Bind
		(VariableList (Variable "$a") (Variable "$b"))
		(Present
			(List input-query (List input-query (Variable "$b"))))
		(List (Variable "$a") (Variable "$b"))))

(define tree-query1 (tree-query query1))
(define tree-query2 (tree-query query2))

; (cog-execute! tree-query1)
; (cog-execute! tree-query2)

(define expect-q1 (Set (List (Concept "A") (Concept "B"))))
(define expect-q2 (Set (List (Similarity (Concept "A")) (Concept "B"))))
