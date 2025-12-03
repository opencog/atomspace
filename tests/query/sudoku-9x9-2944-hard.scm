(use-modules (opencog))
(use-modules (opencog exec))

;
; Definition for a specific puzzle
; Hand-typed-in version of
; http://www.theguardian.com/lifeandstyle/2014/oct/17/sudoku-2944-hard
;
; Certain fixed numbers appear in certain fixed cell locations.
(Edge (Predicate "fix12:8") (Concept "eight"))
(Edge (Predicate "fix15:7") (Concept "seven"))
(Edge (Predicate "fix18:6") (Concept "six"))

(Edge (Predicate "fix21:1") (Concept "one"))
(Edge (Predicate "fix24:6") (Concept "six"))
(Edge (Predicate "fix26:8") (Concept "eight"))
(Edge (Predicate "fix29:7") (Concept "seven"))

(Edge (Predicate "fix33:5") (Concept "five"))
(Edge (Predicate "fix37:1") (Concept "one"))

(Edge (Predicate "fix42:3") (Concept "three"))
(Edge (Predicate "fix48:4") (Concept "four"))

(Edge (Predicate "fix51:4") (Concept "four"))
(Edge (Predicate "fix59:9") (Concept "nine"))

(Edge (Predicate "fix62:9") (Concept "nine"))
(Edge (Predicate "fix68:5") (Concept "five"))

(Edge (Predicate "fix73:4") (Concept "four"))
(Edge (Predicate "fix77:6") (Concept "six"))

(Edge (Predicate "fix81:9") (Concept "nine"))
(Edge (Predicate "fix84:5") (Concept "five"))
(Edge (Predicate "fix86:4") (Concept "four"))
(Edge (Predicate "fix89:1") (Concept "one"))

(Edge (Predicate "fix92:7") (Concept "seven"))
(Edge (Predicate "fix95:3") (Concept "three"))
(Edge (Predicate "fix98:8") (Concept "eight"))

(define (puzzle-9x9-2944-hard)
	(CollectionOf
	(QueryLink
		; There are eighty-one variables! 81 = 9x9 cells
		(VariableList
			(map
				(lambda (var) (TypedVariable var (Type 'Concept)))
				(variable-decls)))
		(AndLink
			; For this puzzle, 24 of the variables are fixed immediately.
			(Edge (Predicate "fix12:8") (Variable "$cell_12"))
			(Edge (Predicate "fix15:7") (Variable "$cell_15"))
			(Edge (Predicate "fix18:6") (Variable "$cell_18"))

			(Edge (Predicate "fix21:1") (Variable "$cell_21"))
			(Edge (Predicate "fix24:6") (Variable "$cell_24"))
			(Edge (Predicate "fix26:8") (Variable "$cell_26"))
			(Edge (Predicate "fix29:7") (Variable "$cell_29"))

			(Edge (Predicate "fix33:5") (Variable "$cell_33"))
			(Edge (Predicate "fix37:1") (Variable "$cell_37"))

			(Edge (Predicate "fix42:3") (Variable "$cell_42"))
			(Edge (Predicate "fix48:4") (Variable "$cell_48"))

			(Edge (Predicate "fix51:4") (Variable "$cell_51"))
			(Edge (Predicate "fix59:9") (Variable "$cell_59"))

			(Edge (Predicate "fix62:9") (Variable "$cell_62"))
			(Edge (Predicate "fix68:5") (Variable "$cell_68"))

			(Edge (Predicate "fix73:4") (Variable "$cell_73"))
			(Edge (Predicate "fix77:6") (Variable "$cell_77"))

			(Edge (Predicate "fix81:9") (Variable "$cell_81"))
			(Edge (Predicate "fix84:5") (Variable "$cell_84"))
			(Edge (Predicate "fix86:4") (Variable "$cell_86"))
			(Edge (Predicate "fix89:1") (Variable "$cell_89"))

			(Edge (Predicate "fix92:7") (Variable "$cell_92"))
			(Edge (Predicate "fix95:3") (Variable "$cell_95"))
			(Edge (Predicate "fix98:8") (Variable "$cell_98"))

			; Aside from the above 24 constraints, there are another
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
