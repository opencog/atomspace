;
; map.scm -- Using MapLink to extract and rewrite.
;
; The MapLink implements a link type analogous to the `map` function
; commonly found in functional programming languages, such as the
; scheme srfi-1 `map`, or `map` in haskell.
;
; In many ways, MapLink is similar to BindLink, except that MapLink
; does not search the entire AtomSpace for matching patterns; rather,
; it only examines the given input list/set, and applies the map to
; that.
;
; In many ways, MapLink is the opposite of PutLink, in that it un-does
; a beta reduction, by extracting values from a matching pattern. Thus,
; MapLink could have been named UnPutLink, CoPutLink or ExtractLink or
; UnBetaReduceLink. That is, MapLink is an adjoint functor to PutLink.
;
; These ideas are illustrated below. The first 4 examples illustrate
; the extraction of values for a single variable; this is, of un-beta-
; reducing a single composition.  This includes a demonstration of
; type checking, which can be used to implement filtering.  The next
; few examples show how multi-variable extraction works, as a straight-
; forward extension of the single-variable case.  The final examples
; illustrate actual "mapping", that is, graph re-writing or graph
; transformation as a result of applying a mapping function.
;
(use-modules (opencog) (opencog exec))

(define single
	(Map
		; Define a pattern that will be used to extract a value
		; from a pattern. The extracted value will correspond to
		; the place-holder variable $x.
		(Scope
			(Variable "$x")
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		; The graph from which a value is to be extracted.  Clearly,
		; the variable $x corresponds to Concept "baz"
		(Evaluation
			(Predicate "foo")
			(List (Concept "bar") (Concept "baz"))))
)

; This should return (Concept "baz") as that is the extracted value
; for the variable $x.
(cog-execute! single)

(define single-set
	(Map
		; The same pattern as above.  Extracts values for variable $x.
		(Scope
			(Variable "$x")
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(Set
			; A set of graphs to which the above pattern should
			; be matched.
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
		))
)

; This should return
; (SetLink (Concept "ah one") (Concept "ah two") (Number 3))
; since these are the three values that correspond to the variable $x.
; Note that SetLinks are unordered links, and so the members of the
; set may be returned in a different order than in which they were
; specified as inputs.
(cog-execute! single-set)


; This example is like the above, except that a ListLink instead of
; a SetLink is used. The ListLink is an ordered link; the ordering
; of the list is preserved by the mapping.
(define single-list
	(Map
		(Scope
			(Variable "$x")
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(List
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
		))
)

; This should return
;    (List (Concept "ah one") (Number 3) (Concept "ah two"))
; Note that the sequential order of the original list is preserved.
(cog-execute! single-list)


; Same as above, except that a type constraint is applied.
; This causes the input to be filtered, so that any graphs
; that don't match the type are discarded.
(define single-type
	(Map
		(Scope
			; The type of the variable MUST be ConceptNode!!
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(Set
			; A set of graphs to which the above pattern should be
			; applied.  Note that, due to the type constraint, only
			; two of the three can match. (Numbers not being Concepts)
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)

; The expected return value is
; (SetLink (ConceptNode "ah one") (ConceptNode "ah two"))
; Note that the NumberNode is no longer included in the result
; of the map.
(cog-execute! single-type)


; The type checking that can be performed during mapping can be
; quite sophisticated. In this example, a single variable $x is
; used to match *every* element in the input set.  However, since
; the variable $x is typed to have a very specific form, some of
; the input set is rejected, as it does not match that type.
;
; This implements a kind of filtering, by returning a subset of
; the original input set, and discarding those values that don't
; match the desired type.  A similar kind of filtering can be done
; using PutLink; see the `filter.scm` example for more.
;
(define single-signature
	(Map
		(Scope
         ; The variable $x must be an evaluation of a certain form!
			(TypedVariable (Variable "$x")
				(Signature
					(Evaluation
						(Predicate "foo")
						(List (Concept "bar") (Type "ConceptNode")))))
			(Variable "$x"))
		(Set
			; Of the three elements in this set, only two have the type
			; that is specified above.
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)

; Executing this will filter the input set down to just two members.
; The return value should be this:
;    (SetLink
;       (EvaluationLink
;          (Predicate "foo")
;          (ListLink (Concept "bar") (Concept "ah one")))
;       (EvaluationLink
;          (Predicate "foo")
;          (ListLink (Concept "bar") (Concept "ah two")))
;    )
;
(cog-execute! single-signature)

; All of the previous examples demonstrated matching to a single
; variable. The below demonstrates matching to two variables, one of
; which is typed to be a concept, and the other a number.
(define double-num-set
	(Map
		(Scope
			; Two variables, $x and $y, both typed.
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y"))))
		(Set
			; Same input as always.
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)

; Extract values for both of the variables.  The expected answer is:
;
;    (SetLink
;       (ListLink (Concept "bar") (Number 3)))
;
; There are four note-worthy things to observe about this answer:
; First, the outermost link is a SetLink; this corresponds to the fact
; that the input to the map was a SetLink. Next, we observe a single
; element in the set, because only one element of the input matched.
; That single element then specifies the values for the two variables.
; The variable values are ordered, in a ListLink, because we need to
; know which value corresponded to $x and which to $y (the first and
; the second, of course).  Without the ListLink, we would not know which
; was which.
;
(cog-execute! double-num-set)

; Same as above, except the variables are typed differently, and
; sometimes, we expect two answers, not one.
(define double-con-set
	(Map
		(Scope
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)

; Much like the example previous, except that there are now two elements
; in the returned set (due to filtering on the types):
;
;    (SetLink
;       (ListLink (Concept "bar") (Concept "ah one"))
;       (ListLink (Concept "bar") (Concept "ah two")))
;
(cog-execute! double-con-set)

; Finally, an example that shows actual mapping taking place!!
; This is a variant of the example immediately above, except that,
; this time, instead of returning two values for two variables,
; the values are used in an ImplicationScopeLink, to perform a graph
; rewrite.  That is, an ImplicationScopeLink is a function P(x)->Q(x),
; so that, if P(x) matches the input P(v), then Q(v) is returned
; (with the value `v` substituted for the variable `x` in Q(x)).
; Actually, this example uses two variables, so the implication
; link is in the form of P(x,y)->Q(x,y) and inputs P(a,b) are
; re-written to Q(a,b).
;
; Observe that the re-writing could also be achieved by combining
; the results of the MapLink with a PutLink.  The form below is
; slightly less verbose, and thus, maybe more convenient than
; using Map and Put together.
;
(define imply-map
	(Map
		; The ImplicationScope is the "map" that will be applied.
		(ImplicationScope
			; The implication has two variables in it, both typed.
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			; The P(x,y) part of the implication.
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y")))
			; The Q(x,y) part of the implication.
			(Evaluation
				(Predicate "reverse-foo")
				(List (Variable "$y") (Variable "$x"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)

; Executing the implication P(x,y)->Q(x,y) on P(a,b) should return
; Q(a,b). In this example, P is `foo` and Q(x,y) is `reverse-foo(y,x)`
; i.e. the order of the arguments is switched. The expected result is:
;
;    (SetLink
;       (EvaluationLink
;          (PredicateNode "reverse-foo")
;          (ListLink (ConceptNode "ah one") (ConceptNode "bar")))
;       (EvaluationLink
;          (PredicateNode "reverse-foo")
;          (ListLink (ConceptNode "ah two") (ConceptNode "bar")))
;    )
;
(cog-execute! imply-map)

(define summation
	(Map
		(ImplicationScope
			(VariableList
				(TypedVariable (Variable "$x") (Type "NumberNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y")))
			(Plus (Variable "$y") (Variable "$x")))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Number 2) (Number 3)))
			(Evaluation
				(Predicate "foo")
				(List (Number 10) (Times (Number 3) (Number 2))))
		))
)

; This example is currently broken, because lazy evaluation does not
; work!
(cog-execute! summation)
