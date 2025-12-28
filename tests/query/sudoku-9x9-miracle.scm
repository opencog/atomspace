(use-modules (opencog))

;
; The famous "Miracle Sudoku" puzzle by Mitchell Lee.
; This puzzle has only TWO given digits but is uniquely solvable
; due to the miracle constraints (anti-knight, anti-king, non-consecutive).
;
;  . . . | . . . | . . .
;  . . . | . . . | . . .
;  . . . | . . . | . . .
; -------+-------+------
;  . . . | . . . | . . .
;  . . 1 | . . . | . . .
;  . . . | . . . | 2 . .
; -------+-------+------
;  . . . | . . . | . . .
;  . . . | . . . | . . .
;  . . . | . . . | . . .
;
; Only two fixed cells:
(Edge (Predicate "fix53:1") (Concept "one"))
(Edge (Predicate "fix67:2") (Concept "two"))

(define (puzzle-9x9-miracle)
	(CollectionOf
	(QueryLink
		; There are eighty-one variables! 81 = 9x9 cells
		(VariableList
			(map
				(lambda (var) (TypedVariable var (Type 'Concept)))
				(variable-decls)))
		(AndLink
			; For this puzzle, only 2 of the variables are fixed.
			(Edge (Predicate "fix53:1") (Variable "$cell_53"))
			(Edge (Predicate "fix67:2") (Variable "$cell_67"))

			; Standard sudoku constraints plus miracle constraints
			(miracle-sudoku-constraints)
		)
		; The solution
		(ListLink (variable-decls))
	)
	)
)
