(use-modules (opencog))
(use-modules (opencog exec))

;
; Definition for a specific puzzle
; This is the "Inkala" puzzle, claimed to be one of the hardest Sudoku
; puzzles ever created.
;
;  1 . . | . . 7 | . 9 .
;  . 3 . | . 2 . | . . 8
;  . . 9 | 6 . . | 5 . .
; -------+-------+------
;  . . 5 | 3 . . | 9 . .
;  . 1 . | . 8 . | . . 2
;  6 . . | . . 4 | . . .
; -------+-------+------
;  3 . . | . . . | . 1 .
;  . 4 . | . . . | . . 7
;  . . 7 | . . . | 3 . .
;
; Certain fixed numbers appear in certain fixed cell locations.
; Row 1
(Edge (Predicate "fix11:1") (Concept "one"))
(Edge (Predicate "fix16:7") (Concept "seven"))
(Edge (Predicate "fix18:9") (Concept "nine"))

; Row 2
(Edge (Predicate "fix22:3") (Concept "three"))
(Edge (Predicate "fix25:2") (Concept "two"))
(Edge (Predicate "fix29:8") (Concept "eight"))

; Row 3
(Edge (Predicate "fix33:9") (Concept "nine"))
(Edge (Predicate "fix34:6") (Concept "six"))
(Edge (Predicate "fix37:5") (Concept "five"))

; Row 4
(Edge (Predicate "fix43:5") (Concept "five"))
(Edge (Predicate "fix44:3") (Concept "three"))
(Edge (Predicate "fix47:9") (Concept "nine"))

; Row 5
(Edge (Predicate "fix52:1") (Concept "one"))
(Edge (Predicate "fix55:8") (Concept "eight"))
(Edge (Predicate "fix59:2") (Concept "two"))

; Row 6
(Edge (Predicate "fix61:6") (Concept "six"))
(Edge (Predicate "fix66:4") (Concept "four"))

; Row 7
(Edge (Predicate "fix71:3") (Concept "three"))
(Edge (Predicate "fix78:1") (Concept "one"))

; Row 8
(Edge (Predicate "fix82:4") (Concept "four"))
(Edge (Predicate "fix89:7") (Concept "seven"))

; Row 9
(Edge (Predicate "fix93:7") (Concept "seven"))
(Edge (Predicate "fix97:3") (Concept "three"))

(define (puzzle-9x9-inkala)
	(CollectionOf
	(QueryLink
		; There are eighty-one variables! 81 = 9x9 cells
		(VariableList
			(map
				(lambda (var) (TypedVariable var (Type 'Concept)))
				(variable-decls)))
		(AndLink
			; For this puzzle, 21 of the variables are fixed immediately.
			; Row 1
			(Edge (Predicate "fix11:1") (Variable "$cell_11"))
			(Edge (Predicate "fix16:7") (Variable "$cell_16"))
			(Edge (Predicate "fix18:9") (Variable "$cell_18"))

			; Row 2
			(Edge (Predicate "fix22:3") (Variable "$cell_22"))
			(Edge (Predicate "fix25:2") (Variable "$cell_25"))
			(Edge (Predicate "fix29:8") (Variable "$cell_29"))

			; Row 3
			(Edge (Predicate "fix33:9") (Variable "$cell_33"))
			(Edge (Predicate "fix34:6") (Variable "$cell_34"))
			(Edge (Predicate "fix37:5") (Variable "$cell_37"))

			; Row 4
			(Edge (Predicate "fix43:5") (Variable "$cell_43"))
			(Edge (Predicate "fix44:3") (Variable "$cell_44"))
			(Edge (Predicate "fix47:9") (Variable "$cell_47"))

			; Row 5
			(Edge (Predicate "fix52:1") (Variable "$cell_52"))
			(Edge (Predicate "fix55:8") (Variable "$cell_55"))
			(Edge (Predicate "fix59:2") (Variable "$cell_59"))

			; Row 6
			(Edge (Predicate "fix61:6") (Variable "$cell_61"))
			(Edge (Predicate "fix66:4") (Variable "$cell_66"))

			; Row 7
			(Edge (Predicate "fix71:3") (Variable "$cell_71"))
			(Edge (Predicate "fix78:1") (Variable "$cell_78"))

			; Row 8
			(Edge (Predicate "fix82:4") (Variable "$cell_82"))
			(Edge (Predicate "fix89:7") (Variable "$cell_89"))

			; Row 9
			(Edge (Predicate "fix93:7") (Variable "$cell_93"))
			(Edge (Predicate "fix97:3") (Variable "$cell_97"))

			; Aside from the above 21 constraints, there are another
			; there are 81+27 constraints. 81 of these say that each
			; of the 81 variables must be a number.  The remaining 27
			; constraints state that nine columns, rows and boxes must
			; have the right form.
			(sudoku-constraints)
		)
		; The solution
		(ListLink (variable-decls))
	)
	)
)
