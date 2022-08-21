;
; table.scm -- Formulas applied to Values from a CSV/TSV table.
;
; This is similar to the `flows.scm` demo, except that the values
; are feteched from a convetional DSV (delimiter-separated-value)
; table. The demo is in two parts. The first part reads the table,
; (a one-liner) and explores how it is represented in the AtomSpace.
; The second part applies some formulas to the table columns.
;
; The second part of the demo is intereasting, because it shows how
; functions, written in Atomese, can be applied to tables, and how
; a "utility function" or a "scoring function" can be written.
; Utility functions are commonly used in machine learning, they
; provide a grand-total score that can be maximized or minized during
; training. The interesting point here is that the scoring function
; is represented in Atomese: it is some tree, some DAG of inputs.
; These trees can be randomly generated and mutated, thus allowing
; genetic-programming algorithms to be implemented in the AtomSpace.
;
; This is. of course, exactly what AS-MOSES does. This is effectively
; a demo of a sub-component of the AS-MOSES subsystem.
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
; Note that `cog-value` and `cog-execute! ValueOf` return the same thing:
(cog-value tab (PredicateNode "flt1"))
(cog-execute! (ValueOf tab (PredicateNode "flt1")))

; Take the difference of two columns. Note that `FloatValueOf` is
; used instead of `ValueOf`, so that the type-checking subsystem
; is happy about the types passed to the operator.
(cog-execute!
	(Minus
		(FloatValueOf tab (PredicateNode "flt2"))
		(FloatValueOf tab (PredicateNode "flt1"))))

; The above can be wrapped into a function. Several examples follow,
; below. First, a function that takes the table as an argument,
; subtracts to columns, and places the result in a third column.
; The column names are hard-coded in the function.

(DefineLink
	(DefinedSchema "col diffs")
   (Lambda
      (Variable "$tbl-name")
		(SetValue
			(Variable "$tbl-name") (Predicate "f2 minus f1")
			(Minus
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt2"))
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt1"))))))

; Apply the function to the table.
(cog-execute! (Put (DefinedSchema "col diffs") tab))

; Verify that the new column showed up.
(cog-keys tab)

; .. and that it contains the expected data.
(cog-value tab (Predicate "f2 minus f1"))

;--------
; The AccumulateLink can be used to sum up all of the rows in a column.
(cog-execute!
	(Accumulate (FloatValueOf tab (Predicate "f2 minus f1"))))

; This can be turned into a simple scoring function. It computes the
; sum-total of the difference of two columns. This is a score, in that
; it is a single number that can be used as a utility function in
; conventional machine-learning algos.
(DefineLink
	(DefinedSchema "compute score")
   (Lambda
      (Variable "$tbl-name")
		(Accumulate
			(Minus
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt2"))
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt1"))))))

; Apply the function to the table.
(cog-execute! (Put (DefinedSchema "compute score") tab))

; That's all, folks.
; -------------------------------------------------------------------
