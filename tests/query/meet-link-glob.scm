; https://github.com/opencog/atomspace/issues/2400

(use-modules ((opencog exec)))

(ListLink
  (ConceptNode "glib")
  (PredicateNode "blab"))

(define basic-get
(Meet
  (ListLink
    (GlobNode "$x"))))

(define typed-get
  (Meet
    (TypedVariableLink
      (GlobNode "$x")
      (TypeNode "ConceptNode"))
    (ListLink
      (GlobNode "$x"))))

(define type-choice-get
  (Meet
    (TypedVariableLink
      (GlobNode "$x")
      (TypeChoice
        (TypeNode "ConceptNode")
        (TypeNode "PredicateNode")))
    (ListLink
      (GlobNode "$x"))))
