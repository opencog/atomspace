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
(Edge (Predicate "fix11") (Concept "one"))
(Edge (Predicate "fix16") (Concept "seven"))
(Edge (Predicate "fix18") (Concept "nine"))

; Row 2
(Edge (Predicate "fix22") (Concept "three"))
(Edge (Predicate "fix25") (Concept "two"))
(Edge (Predicate "fix29") (Concept "eight"))

; Row 3
(Edge (Predicate "fix33") (Concept "nine"))
(Edge (Predicate "fix34") (Concept "six"))
(Edge (Predicate "fix37") (Concept "five"))

; Row 4
(Edge (Predicate "fix43") (Concept "five"))
(Edge (Predicate "fix44") (Concept "three"))
(Edge (Predicate "fix47") (Concept "nine"))

; Row 5
(Edge (Predicate "fix52") (Concept "one"))
(Edge (Predicate "fix55") (Concept "eight"))
(Edge (Predicate "fix59") (Concept "two"))

; Row 6
(Edge (Predicate "fix61") (Concept "six"))
(Edge (Predicate "fix66") (Concept "four"))

; Row 7
(Edge (Predicate "fix71") (Concept "three"))
(Edge (Predicate "fix78") (Concept "one"))

; Row 8
(Edge (Predicate "fix82") (Concept "four"))
(Edge (Predicate "fix89") (Concept "seven"))

; Row 9
(Edge (Predicate "fix93") (Concept "seven"))
(Edge (Predicate "fix97") (Concept "three"))

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
			(Edge (Predicate "fix11") (Variable "$cell_11"))
			(Edge (Predicate "fix16") (Variable "$cell_16"))
			(Edge (Predicate "fix18") (Variable "$cell_18"))

			; Row 2
			(Edge (Predicate "fix22") (Variable "$cell_22"))
			(Edge (Predicate "fix25") (Variable "$cell_25"))
			(Edge (Predicate "fix29") (Variable "$cell_29"))

			; Row 3
			(Edge (Predicate "fix33") (Variable "$cell_33"))
			(Edge (Predicate "fix34") (Variable "$cell_34"))
			(Edge (Predicate "fix37") (Variable "$cell_37"))

			; Row 4
			(Edge (Predicate "fix43") (Variable "$cell_43"))
			(Edge (Predicate "fix44") (Variable "$cell_44"))
			(Edge (Predicate "fix47") (Variable "$cell_47"))

			; Row 5
			(Edge (Predicate "fix52") (Variable "$cell_52"))
			(Edge (Predicate "fix55") (Variable "$cell_55"))
			(Edge (Predicate "fix59") (Variable "$cell_59"))

			; Row 6
			(Edge (Predicate "fix61") (Variable "$cell_61"))
			(Edge (Predicate "fix66") (Variable "$cell_66"))

			; Row 7
			(Edge (Predicate "fix71") (Variable "$cell_71"))
			(Edge (Predicate "fix78") (Variable "$cell_78"))

			; Row 8
			(Edge (Predicate "fix82") (Variable "$cell_82"))
			(Edge (Predicate "fix89") (Variable "$cell_89"))

			; Row 9
			(Edge (Predicate "fix93") (Variable "$cell_93"))
			(Edge (Predicate "fix97") (Variable "$cell_97"))

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
