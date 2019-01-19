;
; get-put.scm -- The two halves of a query.
;
; The BindLink example showed how one can create a query, run it, and
; simulataneously use the query results to create new data in the
; AtomSpace. In fact, this process can be split into two: a "Get"
; part that performs the query, and a "Put" part that performs the
; graph re-writing.
;
; If BindLink is thought of as logical implication:
;    For all x, P(x) implies Q(x)
; then GetLink is the first half:
;    For all x, P(x) implies a set {all x that satisfy P(x)}
; while PutLink is the second half:
;    Create the set {Q(x)} given some other set {x}
; The PutLink is a form of "beta-reduction" or "substitution" or
; "pasting": for each `x` in the set {x} it just pastes `x` into `Q(x)`.
;
; Every BindLink is equivalent to a Get-Put pair. This example
; demonstrates this explicitly.
;
; Spliting a query into a satisfying set, followed by a beta-reduction
; can sometimes allow a more flexible approach to managing the
; knowledgebase.
;
(use-modules (opencog) (opencog exec) (opencog query))

; Place some data into the atomspace. This is the same as in the
; BindLink example.
;
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "pottery")))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "statue")))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "brick")))

(Evaluation (Predicate "from") (List (Concept "make") (Concept "clay")))

(define make-semantic-triple
	(BindLink
		(VariableList   ; Variable declaration (optional)
			(Variable "$var0")
			(Variable "$var1")
			(Variable "$verb")
		)

		; The premise of the implication is wrapped by an AndLink.
		; This means that each of the clauses must be satisfied in
		; the atomspace. In SQL terms, this is an "inner join".
		(AndLink
			; Look for _obj($verb, $var0)
			(Evaluation
				(Predicate "_obj")
				(ListLink
					(Variable "$verb") ; This will match: (Concept "make")
					(Variable "$var0") ; This will match: (Concept "pottery")
				)
			)
			; Look for from($verb, $var1)
			(EvaluationLink
				(Predicate "from")
				(ListLink
					(Variable "$verb") ; This will match: (Concept "make")
					(Variable "$var1") ; This will match: (Concept "clay")
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

; Run the pattern matcher. This matches both required clauses,
; and creates a set of all matching results.
(cog-execute! make-semantic-triple)

; The following should have been printed:
;
; (SetLink
;     (Evaluation (Predicate "make_from")
;         (ListLink (Concept "pottery") (Concept "clay"))))
;
