(use-modules (opencog))
(use-modules (opencog exec))

;
; Definition for a specific puzzle
; Hand-typed-in version of
; http://www.theguardian.com/lifeandstyle/2014/oct/17/sudoku-2944-hard
;
; Certain fixed numbers appear in certain fixed cell locations.
(Edge (Predicate "fix12") (Concept "eight"))
(Edge (Predicate "fix15") (Concept "seven"))
(Edge (Predicate "fix18") (Concept "six"))

(Edge (Predicate "fix21") (Concept "one"))
(Edge (Predicate "fix24") (Concept "six"))
(Edge (Predicate "fix26") (Concept "eight"))
(Edge (Predicate "fix29") (Concept "seven"))

(Edge (Predicate "fix33") (Concept "five"))
(Edge (Predicate "fix37") (Concept "one"))

(Edge (Predicate "fix42") (Concept "three"))
(Edge (Predicate "fix48") (Concept "four"))

(Edge (Predicate "fix51") (Concept "four"))
(Edge (Predicate "fix59") (Concept "nine"))

(Edge (Predicate "fix62") (Concept "nine"))
(Edge (Predicate "fix68") (Concept "five"))

(Edge (Predicate "fix73") (Concept "four"))
(Edge (Predicate "fix77") (Concept "six"))

(Edge (Predicate "fix81") (Concept "nine"))
(Edge (Predicate "fix84") (Concept "five"))
(Edge (Predicate "fix86") (Concept "four"))
(Edge (Predicate "fix89") (Concept "one"))

(Edge (Predicate "fix92") (Concept "seven"))
(Edge (Predicate "fix95") (Concept "three"))
(Edge (Predicate "fix98") (Concept "eight"))

(define (puzzle)
	(CollectionOf
	(QueryLink
		; There are eighty-one variables! 81 = 9x9 cells
		(VariableList
			(map
				(lambda (var) (TypedVariable var (Type 'Concept)))
				(variable-decls)))
		(AndLink
			; For this puzzle, 24 of the variables are fixed immediately.
			(Edge (Predicate "fix12") (Variable "$cell_12"))
			(Edge (Predicate "fix15") (Variable "$cell_15"))
			(Edge (Predicate "fix18") (Variable "$cell_18"))

			(Edge (Predicate "fix21") (Variable "$cell_21"))
			(Edge (Predicate "fix24") (Variable "$cell_24"))
			(Edge (Predicate "fix26") (Variable "$cell_26"))
			(Edge (Predicate "fix29") (Variable "$cell_29"))

			(Edge (Predicate "fix33") (Variable "$cell_33"))
			(Edge (Predicate "fix37") (Variable "$cell_37"))

			(Edge (Predicate "fix42") (Variable "$cell_42"))
			(Edge (Predicate "fix48") (Variable "$cell_48"))

			(Edge (Predicate "fix51") (Variable "$cell_51"))
			(Edge (Predicate "fix59") (Variable "$cell_59"))

			(Edge (Predicate "fix62") (Variable "$cell_62"))
			(Edge (Predicate "fix68") (Variable "$cell_68"))

			(Edge (Predicate "fix73") (Variable "$cell_73"))
			(Edge (Predicate "fix77") (Variable "$cell_77"))

			(Edge (Predicate "fix81") (Variable "$cell_81"))
			(Edge (Predicate "fix84") (Variable "$cell_84"))
			(Edge (Predicate "fix86") (Variable "$cell_86"))
			(Edge (Predicate "fix89") (Variable "$cell_89"))

			(Edge (Predicate "fix92") (Variable "$cell_92"))
			(Edge (Predicate "fix95") (Variable "$cell_95"))
			(Edge (Predicate "fix98") (Variable "$cell_98"))

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
