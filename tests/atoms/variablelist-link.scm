
(use-modules (opencog) (opencog exec))

(EvaluationLink
    (PredicateNode "foo")
    (ListLink (ConceptNode "bingo") (ConceptNode "yes!")))

(EvaluationLink
    (AnchorNode "bar")
    (ListLink (ConceptNode "hurrah") (ConceptNode "yay!")))

(EvaluationLink
    (ConceptNode "baz")
    (ListLink (ConceptNode "oops") (ConceptNode "Oh no, Mr. Bill!")))


(DefineLink
    (DefinedType "predicate-type")
    (Signature
        (EvaluationLink
            (TypeChoice
                (TypeNode "PredicateNode")
                (TypeNode "AnchorNode"))
            (ListLink
                (Type "ConceptNode") (Type "ConceptNode")))))

(define predicate-search
    (GetLink
        (TypedVariable
            (Variable "$x")
            (DefinedType "predicate-type"))
        (AndLink (Variable "$x"))))
