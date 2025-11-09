;
; flows.scm -- Flowing Values between Atoms.
;
; Atoms can be thought of as "pipes", and Values as the thing that
; "flows through the pipes".  This is a reasonable analogy, because
; Atoms are fairly heavyweight and are immutable, and require a lot of
; machinery to be placed in the AtomSpace. That machinery is required
; in order to be able to search (perform queries) over Atoms. Values,
; by contrast, are much smaller and simpler. They are mutable,
; ephemeral, and can change rapidly. (The price for this: they cannot
; be searched!).
;
; But how does the "fluid" flow in the "pipes"? The example below walks
; through ways in which Values can be pulled out of specific Atoms, then
; transformed or mutated in some specific way, and then re-injected.
; In these examples, the mutations are arithmetic formulas that are
; applied to the Values. The formulas themselves are expressed as
; Atomese.
;
; The goal of having formulas in Atomese is that such formulas, rather
; than being hard-coded in Python, C++ or scheme, can instead be
; imported from either existing datasets (such as the Systems Biology
; Markup Language) or they can be obtained by machine learning (such
; as MOSES).
;
; See the `flow-formulas.scm` example for how to work with dynamically
; updating flows.

(use-modules (opencog) (opencog exec))

; The Value is located at key (Predicate "*-TruthValueKey-*")
(define tvkey (Predicate "*-TruthValueKey-*"))

; An atom with a FloatValue on it...
(cog-set-value! (Concept "foo") tvkey (FloatValue 0.3 0.7))

; This is how we get it's Value ...
(cog-execute! (ValueOf (Concept "foo") tvkey))

; Transfer the FloatValue from "foo" to "bar" ... copy it.
(cog-execute!
	(SetValue (Concept "bar") tvkey
		(ValueOf (Concept "foo") tvkey)))

; Verify that the TV on "bar" has changed.
(cog-value (Concept "bar") tvkey)

; SetValue is interesting because it allows complex arithmetic expressions
; to be specified in Atomese. Below, simply take the square of the TV.
(cog-execute!
	(SetValue
		(Concept "bar") tvkey
		(Times
			(FloatValueOf (Concept "foo") tvkey)
			(FloatValueOf (Concept "foo") tvkey))))

; Formulas can be used to compute TV's, as shown in the `formula.scm`
; example. Consider a named formula, with variables.
(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

(DefineLink
   (DefinedProcedure "has a reddish color")
	(Lambda
		(VariableList (Variable "$X") (Variable "$Y"))
		(FloatColumn
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y"))))
			(Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y"))))))

; Some data...
(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.98))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.9 0.98))

; Use the formula to compute a new TV, and attach that TV to some Atom.
; This is little more than the copy above, except that the evaluation
; is actually performed, so that the new TV is computed, before being
; copied. In general, if the third Atom passed to SetValue is executable,
; then it will be executed to obtain the TV.
(cog-execute!
	(SetValue
		(Concept "bar") tvkey
		(ExecutionOutput
			(DefinedProcedure "has a reddish color")
			(List (Concept "A") (Concept "B")))))

; That the above really does flow the TV from one place to another can
; be seen by looking at dynamic changes. So -- change the TV on A,
; and recompute...
(cog-set-value! (Concept "A") tvkey (FloatValue 0.8 0.9))
(cog-execute!
	(SetValue
		(Concept "bar") tvkey
		(ExecutionOutput
			(DefinedProcedure "has a reddish color")
			(List (Concept "A") (Concept "B")))))

; In many ways, the SetValueLink behaves a lot like a generalized
; EvaluationLink. So: normally, an EvaluationLink consists of a
; predicate, and the list of arguments that it applies to. The
; SetTVLink is similar, except that it couples the predicate to
; the target Atom that it should apply to.  This can be seen in
; the equivalent form, below.
(cog-execute!
	(SetValue
		(Concept "bar") tvkey
		(DefinedProcedure "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; -----------------------------------------------------------
; Everything demonstrated above can be done in a generalized way, for
; arbitrary values. The primary difference is that the SetValueLink is
; used for this, and a key must be provided as an additional argument.

(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define key (Predicate "some key"))
(define kee (Predicate "other key"))

; Start by setting a value in the "traditional fashion"
(cog-set-value! foo key (FloatValue 1 2 3 4 5))

; Take a look at it
(cog-execute! (ValueOf foo key))

; Copy from foo to bar
(cog-execute! (SetValue bar kee (ValueOf foo key)))

; Take a look at it
(cog-execute! (ValueOf bar kee))

; Try out some math
(cog-execute! (SetValue bar kee
	(Times (FloatValueOf foo key) (FloatValueOf foo key))))

; Verify
(cog-execute! (ValueOf bar kee))

; Define a procedure that computes N(N+1)/2 aka a "triangle number".
; A Procedure is used, instead of a DefinedSchema, since, in principle,
; DefinedSchema should be limited to returning only Atoms, and not
; Values in general.
(DefineLink
   (DefinedProcedure "triangle numbers")
	(Lambda
		(Variable "$X")
		(Divide
			(Times (Variable "$X") (Plus (Variable "$X") (Number 1)))
			(Number 2))))

; Apply the schema to a vector of numbers, and attach the result to
; the bar Atom, much as before.
(cog-execute!
	(SetValue bar kee
		(DefinedProcedure "triangle numbers")
		(FloatValueOf foo key)))
;
; -------- THE END -----------
