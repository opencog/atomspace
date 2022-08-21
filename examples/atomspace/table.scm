;
; table.scm -- Formulas applied to Values from a CSV/TSV table.
;
; This is similar to the `flows.scm` demo, except that the values
; are feteched from a convetional DSV (delimiter-separated-value)
; table. The demo is in two parts. The first part reads the table,
; (a one-liner) and explores how it is represented in the AtomSpace.
; The second part applies some formulas to the table columns.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog csv-table))

; Create an Atom on which the table will be located.
(define tab (Concept "My foo Table"))

; Load the table (located in this directory.)
(load-table tab "table.csv")

; Verify that the table loaded. First, take a look at all of the keys:
(cog-keys tab)

; The ordered list of all the columns will be located at the
; "well-known predicate". All tables will have this; it is an
; ordered list of the columns in the table (in the same order
; as the file.)
(define colkeys (Predicate "*-column-keys-*"))
(cog-value tab colkeys)

; Verify that the data for each column is present.
; Loop over the columns, and print the keys and values on them.
(for-each
	(lambda (KEY) 
		(format #t "The key ~A   holds data ~A\n" KEY (cog-value tab KEY)))
	(cog-value->list (cog-value tab colkeys)))
;
; -------------------------------------------------------------------
; Part two: apply some formulas to the columns.
;
; Note that cog-value and cog-execute! ValueOf return the same thing:
(cog-value tab (PredicateNode "flt1"))
(cog-execute! (ValueOf tab (PredicateNode "flt1")))

(cog-execute!
	(Minus
		(ValueOf tab (PredicateNode "flt2"))
		(ValueOf tab (PredicateNode "flt1"))))

; That's all, folks.
; -------------------------------------------------------------------
