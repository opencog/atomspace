;
; Simplified Sudoku puzzle rules, for smaller puzzles than the normal
; 9x9 one. These are encoded in such a way that the pattern matcher
; can try to do a brute-force exploration.
;
(use-modules (opencog))


; Definition of a number.  Cells in the sudoku puzzle can only contain
; numbers.
(Edge (Predicate "IsNumber") (Concept "one"))
(Edge (Predicate "IsNumber") (Concept "two"))
(Edge (Predicate "IsNumber") (Concept "three"))
(Edge (Predicate "IsNumber") (Concept "four"))
(Edge (Predicate "IsNumber") (Concept "five"))
(Edge (Predicate "IsNumber") (Concept "six"))
(Edge (Predicate "IsNumber") (Concept "seven"))
(Edge (Predicate "IsNumber") (Concept "eight"))
(Edge (Predicate "IsNumber") (Concept "nine"))

; -------------------------------------------------------------------
; The set of numbers for the 2x2 puzzle
(Edge
	(Predicate "2x2 sudoku")
	(ExclusiveLink
		(Concept "one")
		(Concept "two")
	)
)

; Four solution constraints for the 2x2 puzzle.
;
; I've avoided using whizzy scheme for-loops to specify these, and
; instead tediously wrote them out by hand.  The goal here is to make
; the structure slightly easier to read and understand.
;
(define (x2_row1)
	(Edge
		(Predicate "2x2 sudoku")
		(ExclusiveLink
			(Variable "$cell_11")
			(Variable "$cell_12")
		)
	)
)
(define (x2_row2)
	(Edge
		(Predicate "2x2 sudoku")
		(ExclusiveLink
			(Variable "$cell_21")
			(Variable "$cell_22")
		)
	)
)

;; Next, column constraints
(define (x2_col1)
	(Edge
		(Predicate "2x2 sudoku")
		(ExclusiveLink
			(Variable "$cell_11")
			(Variable "$cell_21")
		)
	)
)
(define (x2_col2)
	(Edge
		(Predicate "2x2 sudoku")
		(ExclusiveLink
			(Variable "$cell_12")
			(Variable "$cell_22")
		)
	)
)

; The contents of the cells must be numbers!
; This constraint will not be needed, as the requirement that
; the columns, rows and boxes be a number set is sufficient.  So the
; below only adds complexity to the problem, slowing down solving.
(define (cells-are-numbers-2x2)
	(list
		(Edge (Predicate "IsNumber") (Variable "$cell_11"))
		(Edge (Predicate "IsNumber") (Variable "$cell_12"))

		(Edge (Predicate "IsNumber") (Variable "$cell_21"))
		(Edge (Predicate "IsNumber") (Variable "$cell_22"))
	)
)

;; The grand-total set of constraints.
(define (x2-sudoku-constraints)
	(list
		; constraint isn't needed. But we use it anyway, "for fun"
		(cells-are-numbers-2x2) ; useless constraint.
		(x2_row1)
		(x2_row2)
		(x2_col1)
		(x2_col2)
	)
)

; Define the variables to be solved for.
; This is just a big list of all the cells.
;
(define (x2-variable-decls)
	(list
		(Variable "$cell_11")
		(Variable "$cell_12")

		(Variable "$cell_21")
		(Variable "$cell_22")
	)
)

; ------------------------------------------

; Certain fixed numbers appear in certain fixed cell locations.
(Edge (Predicate "x2-fix11") (Concept "one"))
;
; This puzzle should have exactly one solution, since fixing the
; upper-left corner constrains everything else.
(define (x2-puzzle)
	(CollectionOf
	(QueryLink
		(VariableList (x2-variable-decls))
		(AndLink
			; For this puzzle, 1 of the variables is fixed immediately.
			(Edge (Predicate "x2-fix11") (Variable "$cell_11"))

			; Aside from the above constraint, there are another
			; 4 constraints.
			(x2-sudoku-constraints)
		)
		; The solution
		(ListLink (x2-variable-decls))
	)
	)
)

; This puzzle should have 2 solutions total: no cells are fixed, and
; so the constraint rules should explore all two possible solutions.
; These are:
;
;   1 2      2 1
;   2 1      1 2
;
(define (x2-any)
	(CollectionOf
	(QueryLink
		(VariableList (x2-variable-decls))
		(AndLink
			; There are 4 constraints. One is actually redundant...
			(x2-sudoku-constraints)
		)
		; The solution
		(ListLink (x2-variable-decls))
	)
	)
)

; -------------------------------------------------------------------
; -------------------------------------------------------------------
; -------------------------------------------------------------------
; The set of numbers for the 3x3 puzzle
(Edge
	(Predicate "3x3 sudoku")
	(ExclusiveLink
		(Concept "one")
		(Concept "two")
		(Concept "three")
	)
)

; Four solution constraints for the 3x3 puzzle.
;
; I've avoided using whizzy scheme for-loops to specify these, and
; instead tediously wrote them out by hand.  The goal here is to make
; the structure slightly easier to read and understand.
;
(define (x3_row1)
	(Edge
		(Predicate "3x3 sudoku")
		(ExclusiveLink
			(Variable "$cell_11")
			(Variable "$cell_12")
			(Variable "$cell_13")
		)
	)
)
(define (x3_row2)
	(Edge
		(Predicate "3x3 sudoku")
		(ExclusiveLink
			(Variable "$cell_21")
			(Variable "$cell_22")
			(Variable "$cell_23")
		)
	)
)

(define (x3_row3)
	(Edge
		(Predicate "3x3 sudoku")
		(ExclusiveLink
			(Variable "$cell_31")
			(Variable "$cell_32")
			(Variable "$cell_33")
		)
	)
)

;; Next, column constraints
(define (x3_col1)
	(Edge
		(Predicate "3x3 sudoku")
		(ExclusiveLink
			(Variable "$cell_11")
			(Variable "$cell_21")
			(Variable "$cell_31")
		)
	)
)
(define (x3_col2)
	(Edge
		(Predicate "3x3 sudoku")
		(ExclusiveLink
			(Variable "$cell_12")
			(Variable "$cell_22")
			(Variable "$cell_32")
		)
	)
)
(define (x3_col3)
	(Edge
		(Predicate "3x3 sudoku")
		(ExclusiveLink
			(Variable "$cell_13")
			(Variable "$cell_23")
			(Variable "$cell_33")
		)
	)
)

; The contents of the cells must be numbers!
; Actually, this constraint will not be needed, as the requirement that
; the columns, rows and boxes be a number set is sufficient.  So the
; below only adds complexity to the problem, slowing down solving.
(define (cells-are-numbers-3x3)
	(list
		(Edge (Predicate "IsNumber") (Variable "$cell_11"))
		(Edge (Predicate "IsNumber") (Variable "$cell_12"))
		(Edge (Predicate "IsNumber") (Variable "$cell_13"))

		(Edge (Predicate "IsNumber") (Variable "$cell_21"))
		(Edge (Predicate "IsNumber") (Variable "$cell_22"))
		(Edge (Predicate "IsNumber") (Variable "$cell_23"))

		(Edge (Predicate "IsNumber") (Variable "$cell_31"))
		(Edge (Predicate "IsNumber") (Variable "$cell_32"))
		(Edge (Predicate "IsNumber") (Variable "$cell_33"))
	))

;; The grand-total set of constraints.
(define (x3-sudoku-constraints)
	(list
		(cells-are-numbers-3x3) ; constraint isn't needed.
		(x3_row1)
		(x3_row2)
		(x3_row3)
		(x3_col1)
		(x3_col2)
		(x3_col3)
	)
)

; Define the variables to be solved for.
; This is just a big list of all the cells.
;
(define (x3-variable-decls)
	(list
		(Variable "$cell_11")
		(Variable "$cell_12")
		(Variable "$cell_13")

		(Variable "$cell_21")
		(Variable "$cell_22")
		(Variable "$cell_23")

		(Variable "$cell_31")
		(Variable "$cell_32")
		(Variable "$cell_33")
	)
)

; ------------------------------------------
;
; The puzle below is underconstrained, and should have four total
; solutions: two with permuted columns, and two with permuted rows:
;
;  1 2 3
;  2 3 1   is one, then permute the rightmost 2 cols for another, and
;  3 1 2   then perumted the bottom two rows for two more solutions, to
;          get four grand total.  The upper-left 1 is fixed.

; Certain fixed numbers appear in certain fixed cell locations.
(Edge (Predicate "x3-fix11") (Concept "one"))

(define (x3-puzzle)
	(CollectionOf
	(QueryLink
		(VariableList (x3-variable-decls))
		(AndLink
			; For this puzzle, 1 of the variables is fixed immediately.
			(Edge (Predicate "x3-fix11") (Variable "$cell_11"))

			; Aside from the above constraints, there are another
			; 6 constraints.
			(x3-sudoku-constraints)
		)
		; The solution
		(ListLink (x3-variable-decls))
	)
	)
)
