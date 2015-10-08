;
; bindlink-example.scm
;
; Linas Vepstas January 2009
;
; This file contains a simple, visual-inspection demo example for the
; use of the query engine to find pattern matches using the scheme
; interfaces.  The below defines a single BindLink and two chunks of
; data, and then calls the pattern matcher.
;
; The expected result, after running the below, is that the following
; should be printed:
;
; guile> (cog-bind x)
; (ListLink (EvaluationLink (PredicateNode "make_from")
;     (ListLink (ConceptNode "pottery")
;            (ConceptNode "clay"))))
;
; Note the outermost ListLink is simply an enumeration of all of the
; possible results from the implication; in this case, there is only one
; possible result.
;

(use-modules (opencog))
(use-modules (opencog query))
(load-from-path "utilities.scm")

; Create a "semantic triple" by combining a verb, an object and a
; preposition. This searches the atomsace for a verb-object pair,
; and a verb-preposition pair, and if it finds them, creates a triple.
;
; Note: this is a highly simplified example; doing this well, for
; general English language sentences, as parsed by RelEx, is
; considerably more complex.
(define make-semantic-triple
	(BindLink
		(VariableList   ; Optional variable declaration
			(VariableNode "$var0")
			(VariableNode "$var1")
			(VariableNode "$verb")
		)

		; AndLink means each of the clauses must be satisfied in the
		; atomspace.
		(AndLink
			; Look for _obj($verb, $var0)
			(EvaluationLink
				(PredicateNode "_obj")
				(ListLink
					(VariableNode "$verb") ; (ConceptNode "make")
					(VariableNode "$var0") ; (ConceptNode "pottery")
				)
			)
			; Look for from($verb, $var1)
			(EvaluationLink
				(PredicateNode "from")
				(ListLink
					(VariableNode "$verb") ; (ConceptNode "make")
					(VariableNode "$var1") ; (ConceptNode "clay")
				)
			)
		)
		; Combine the two above into one.
		(EvaluationLink
			(PredicateNode "make_from")
			(ListLink
				(VariableNode "$var0")
				(VariableNode "$var1")
			)
		)
	)
)

; Place some data in the atomspace that the above pattern will
; be able to find.
(EvaluationLink (stv 1.0 1.0)
	(PredicateNode "_obj")
	(ListLink
		(ConceptNode "make")
		(ConceptNode "pottery")))

(EvaluationLink  (stv 1.0 1.0)
	(PredicateNode "from")
	(ListLink
		(ConceptNode "make")
		(ConceptNode "clay")))

; Run the pattern matcher.
(cog-bind make-semantic-triple)
