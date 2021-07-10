;
; satisfaction.scm -- Determining satisfiability of a query.
;
; SatisfactionLink usage example.
;
; Using the SatisfactionLink is fairly straightforward; what this
; example shows is how to obtain the variable grounding that caused
; the satisfaction to be fulfilled.
;

(use-modules (opencog) (opencog exec))

; Define a very simple satisfaction link.
(define satlink
	(Satisfaction
		(Evaluation
			(Predicate "foobar")
			(List
				(Concept "funny")
				(Variable "$x")))))

; Create something that will satisfy the above.
(Evaluation
	(Predicate "foobar")
	(List
		(Concept "funny")
		(Concept "thing")))

; Actually run it - this should return TrueTV i.e. `(stv 1 1)`
; because the SatisfactionLink is satisfiable.
(cog-execute! satlink)

; Fine, but can we know what variable grounding resulted in the
; satisfaction? Sure! In several different ways.
;
; Define an anchor point, where the result will be placed.
(define gnd-sat
	(Satisfaction
		(Anchor "please put groundings here")
		(Evaluation
			(Predicate "foobar")
			(List
				(Concept "funny")
				(Variable "$x")))))

(cog-execute! gnd-sat)

; Above returns the same TruthValue as before. The grounding for
; Variable $x is attached to the Anchor with a MemberLink:
(define anchr (Anchor "please put groundings here"))
(cog-incoming-by-type anchr 'Member)


; An alternate way of writing the above, this time declaring the
; the variable explicitly.
(define gnd-decl-sat
	(Satisfaction
		(VariableList
			(Variable "$x")
			(Anchor "please put groundings here"))
		(Evaluation
			(Predicate "foobar")
			(List
				(Concept "funny")
				(Variable "$x")))))

(cog-execute! gnd-decl-sat)
(cog-incoming-by-type anchr 'Member)

; If there is more than one variable, then the order in which
; they appear is important, as it is used to report the grounding.
(define gnd2-sat
	(Satisfaction
		(VariableList
			(Variable "$p")
			(Variable "$x")
			(Anchor "please put groundings here"))
		(Evaluation
			(Variable "$p")
			(List
				(Concept "funny")
				(Variable "$x")))))

(cog-execute! gnd2-sat)
(cog-incoming-by-type anchr 'Member)

(define gnd2-get
	(Get
		(VariableList
			(Variable "$p")
			(Variable "$x"))
		(Evaluation
			(Variable "$p")
			(List
				(Concept "funny")
				(Variable "$x")))))

(cog-execute! gnd2-get)
