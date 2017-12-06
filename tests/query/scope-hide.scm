;
; Unit test for bug opencog/atomspace/issues/950
;

(use-modules (opencog))
(use-modules (opencog exec))

;; The ForAll link is a ScopeLink, and so (VariableNode "$X")
;; is a bound var.
(define forall
	(ForAllLink
		(VariableNode "$X")
		(EvaluationLink (Predicate "P") (VariableNode "$X"))))

;; This has the same variable name as the above -- and so,
;; during pattern matching, this should not be considered to be
;; a self-grounding -- $X should bind to $X just fine.
(define getx (Get (LocalQuote (ForAllLink (Variable "$X") (Variable "$B")))))

;; Alpha conversion means that getv should be exactly the same atom
;; as getx.  Note that chronological order matters: this MUST be
;; defined after the above, for this unit test to actually test
;; something valid.
(define getv (Get (LocalQuote (ForAllLink (Variable "$V") (Variable "$B")))))

;; This is to make sure that the following ill-formed scope link isn't
;; created
;;
;; (ImplicationScopeLink
;;   (TypedVariableLink
;;     (ConceptNode "ChurchOfEngland")
;;     (TypeChoice
;;       (TypeNode "ConceptNode")
;;       (TypeNode "SchemaNode")
;;       (TypeNode "PredicateNode")
;;     )
;;   )
;;   (MemberLink
;;     (ConceptNode "ChurchOfEngland")
;;     (ConceptNode "AnglicanChurch")
;;   )
;;   (EvaluationLink
;;     (PredicateNode "subOrganization")
;;     (ListLink
;;       (ConceptNode "ChurchOfEngland")
;;       (ConceptNode "ChurchOfEngland")
;;     )
;;   )
;; )

(MemberLink
  (ConceptNode "ChurchOfEngland")
  (ConceptNode "AnglicanChurch")
)

(define rule
   (BindLink
      (TypedVariableLink
         (VariableNode "?C")
         (TypeChoice
            (TypeNode "ConceptNode")
            (TypeNode "SchemaNode")
            (TypeNode "PredicateNode")
         )
      )
      (MemberLink
         (VariableNode "?C")
         (ConceptNode "AnglicanChurch")
      )
      (ImplicationScopeLink
         (TypedVariableLink
            (VariableNode "?C")
            (TypeChoice
               (TypeNode "ConceptNode")
               (TypeNode "SchemaNode")
               (TypeNode "PredicateNode")
            )
         )
         (MemberLink
            (VariableNode "?C")
            (ConceptNode "AnglicanChurch")
         )
         (EvaluationLink
            (PredicateNode "subOrganization")
            (ListLink
               (VariableNode "?C")
               (ConceptNode "ChurchOfEngland")
            )
         )
      )
   )
)
