;
; unordered-jswiergo.scm
;
; Unordered link nesting tests, from Jacek Świergocki
; AKA issue #148
;
(use-modules (opencog) (opencog exec))

(define shared (Unordered (Concept "A")))
(List shared (List shared (Concept "B")))

(define query1 (Unordered (Variable "$a")))
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
(define expect-q2 (Set (List (Unordered (Concept "A")) (Concept "B"))))

; -------------------------------------------------------------------
; Similar structure, but more complicated

(List (Unordered (Concept "X"))
	(Unordered (Concept "A1") (Concept "B1"))
	(List (Unordered (Concept "X"))
		(Unordered (Concept "B1") (Concept "A1"))))

(List (Unordered (Concept "X"))
	(Unordered (Concept "A2") (Concept "B2"))
	(List (Unordered (Concept "X"))
		(Unordered (Concept "B2") (Concept "A2"))))

(List (Unordered (Concept "X"))
	(Unordered (Concept "A3") (Concept "B3"))
	(List (Unordered (Concept "X"))
		(Unordered (Concept "B3") (Concept "A3"))))


(define (big-tree-query input-query)
	(Bind
		(VariableList (Variable "$a") (Variable "$b") (Variable "$c"))
		(Present
			(List
				input-query
				(Unordered (Variable "$b") (Variable "$c"))
				(List
					input-query
					(Unordered (Variable "$b") (Variable "$c")))))
		(List (Variable "$a") (Variable "$b") (Variable "$c"))))

(define big-tree-q1 (big-tree-query query1))
(define big-tree-q2 (big-tree-query query2))

; (cog-execute! big-tree-q1)
; (cog-execute! big-tree-q2)

(define big-expect1
	(Set
		(List (Concept "X") (Concept "A1") (Concept "B1"))
		(List (Concept "X") (Concept "A2") (Concept "B2"))
		(List (Concept "X") (Concept "A3") (Concept "B3"))
		(List (Concept "X") (Concept "B1") (Concept "A1"))
		(List (Concept "X") (Concept "B2") (Concept "A2"))
		(List (Concept "X") (Concept "B3") (Concept "A3"))
	))

(define big-expect2
	(Set
		(List (Unordered (Concept "X")) (Concept "A1") (Concept "B1"))
		(List (Unordered (Concept "X")) (Concept "A2") (Concept "B2"))
		(List (Unordered (Concept "X")) (Concept "A3") (Concept "B3"))
		(List (Unordered (Concept "X")) (Concept "B1") (Concept "A1"))
		(List (Unordered (Concept "X")) (Concept "B2") (Concept "A2"))
		(List (Unordered (Concept "X")) (Concept "B3") (Concept "A3"))
	))

*unspecified*
