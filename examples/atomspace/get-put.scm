;
; get-put.scm -- The two halves of a query.
;
; The QueryLink example showed how one can create a query, run it, and
; simultaneously use the query results to create new data in the
; AtomSpace. In fact, this process can be split into two: a "Meet"
; part that performs the query, and a "Put" part that performs the
; graph re-writing.
;
; The QueryLink can be thought of as logical implication:
;	 For all x, P(x) implies Q(x)
; The MeetLink is the first half:
;	 For all x, P(x) implies a set {all x that satisfy P(x)}
; The PutLink is the second half:
;	 Create the set {Q(x)} given some other set {x}
; The PutLink is a form of "beta-reduction" or "substitution" or
; "pasting": for each `x` in the set {x} it just pastes `x` into `Q(x)`.
;
; Every QueryLink is equivalent to a Meet-Put pair. This example
; demonstrates this explicitly.
;
; Splitting a query into a satisfying set, followed by a beta-reduction
; can sometimes allow a more flexible approach to managing the
; knowledgebase. In some sense, Meet and Put are opposites: whatever
; one of them does, the other can undo. In this example, a query is
; made for P(x) and then Q(x) is created. But one could, instead,
; create Q(x) first, and then ask for it later. In the language of
; category theory, Meet and Put are adjoint functors.
;
(use-modules (opencog) (opencog exec))

; Place some data into the atomspace. This is the same as in the
; QueryLink example.
;
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "pottery")))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "statue")))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "brick")))

(Evaluation (Predicate "from") (List (Concept "make") (Concept "clay")))

; Create a query, an "inner join" of several interesting clauses.
;
; This searches for all triples (verb, thing, substance) that
; simultaneously satisfy two clauses:
;
;			_obj(verb, thing) AND from(verb, substance)
;
; This is an "inner join" because `verb` must be the same in both
; clauses.

(define get-satisfying-set
	(MeetLink
		(VariableList	; Variable declaration (optional)
			(Variable "$verb")
			(Variable "$var0")
			(Variable "$var1")
		)

		; The distinct clauses are wrapped by an AndLink.
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
	)
)

; Run the pattern-matcher, to verify that it works. This matches both
; required clauses, and returns a set of all matching results.
(cog-execute! get-satisfying-set)

; Define a beta-reduction.
(define reduction-rule
	(PutLink
		; A variable declaration is mandatory, whenever there is
		; more than one variable. It is required, so that one can
		; know the order (the sequence) of the variables. If there
		; is only one variable, it does not need to be declared.
		(VariableList
			(Variable "$verb")
			(Variable "$var0")
			(Variable "$var1"))

		; The "output" of the re-write; the `Q(x)` part of the
		; implication.
		(Evaluation
			(Predicate "make_from")
			(List (Variable "$var0") (Variable "$var1")))

		; The "input" to the re-write; this will be executed to get
		; a set of `x`'s that will be pasted into `Q(x)`.
		get-satisfying-set
	))

; Now, run the reduction rule.
(cog-execute! reduction-rule)

; There is no need to cache the intermediate values returned by MeetLink.
; They can be piped, dynamically, on-the-fly, to the PutLink.  This
; looks almost identical to the above PutLink, except that the "input"
; is not a UnisetValue, its a MeetLink.
;
(define find-and-rewrite-rule
	(PutLink

		; Using a dollar sign in variables is just a goofy convention.
		; There is no technical need to stick to that convention.
		(VariableList
			(Variable "verb")
			(Variable "thing")
			(Variable "stuff"))

		(Evaluation
			(Predicate "make_from")
			(List (Variable "thing") (Variable "stuff")))

		; This is the MeetLink, defined earlier.
		get-satisfying-set
	))

; Now, run the combined Meet-Put structure:
(cog-execute! find-and-rewrite-rule)

; The results reported by this rule are *identical* to the results
; that the QueryLink rule, from the QueryLink example would report.
; This Meet-Put combination behaves in an identical fashion to a
; single QueryLink.  It just split up the operation into parts.
