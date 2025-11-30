;
; Unit test for bug opencog/atomspace/issues/950
;

(use-modules (opencog))
(use-modules (opencog exec))

;; The Lambda link is a ScopeLink, and so (VariableNode "$X")
;; is a bound var.
(define forall
	(LambdaLink
		(VariableNode "$X")
		(EdgeLink (Predicate "P") (VariableNode "$X"))))

;; This has the same variable name as the above -- and so,
;; during pattern matching, this should not be considered to be
;; a self-grounding -- $X should bind to $X just fine.
(define getx (CollectionOf (Meet (LocalQuote (LambdaLink (Variable "$X") (Variable "$B"))))))

;; Alpha conversion means that getv should be exactly the same atom
;; as getx.  Note that chronological order matters: this MUST be
;; defined after the above, for this unit test to actually test
;; something valid.
(define getv (CollectionOf (Meet (LocalQuote (LambdaLink (Variable "$V") (Variable "$B"))))))

;; This is to make sure that the following ill-formed scope link isn't
;; created
;;
;; (RuleLink
;;   (TypedVariableLink
;;     (ConceptNode "ChurchOfEngland")
;;     (TypeChoice
;;       (TypeNode "ConceptNode")
;;       (TypeNode "SchemaNode")
;;       (TypeNode "PredicateNode")
;;     )
;;   )
;;   (TagLink
;;     (ConceptNode "ChurchOfEngland")
;;     (ConceptNode "AnglicanChurch")
;;   )
;;   (EdgeLink
;;     (PredicateNode "subOrganization")
;;     (ListLink
;;       (ConceptNode "ChurchOfEngland")
;;       (ConceptNode "ChurchOfEngland")
;;     )
;;   )
;; )

(TagLink
  (ConceptNode "ChurchOfEngland")
  (ConceptNode "AnglicanChurch")
)

(define rule
   (CollectionOf
   (QueryLink
      (TypedVariableLink
         (VariableNode "?C")
         (TypeChoice
            (TypeNode "ConceptNode")
            (TypeNode "SchemaNode")
            (TypeNode "PredicateNode")
         )
      )
      (TagLink
         (VariableNode "?C")
         (ConceptNode "AnglicanChurch")
      )
      (RuleLink
         (TypedVariableLink
            (VariableNode "?C")
            (TypeChoice
               (TypeNode "ConceptNode")
               (TypeNode "SchemaNode")
               (TypeNode "PredicateNode")
            )
         )
         (TagLink
            (VariableNode "?C")
            (ConceptNode "AnglicanChurch")
         )
         (EdgeLink
            (PredicateNode "subOrganization")
            (ListLink
               (VariableNode "?C")
               (ConceptNode "ChurchOfEngland")
            )
         )
      )
   )
   )
)
