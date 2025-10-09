
; Some data to populate the atomspace
(SetLink
   (UnorderedLink
      (AssociativeLink
         (ConceptNode "want-this")
         (ConceptNode "valid")
      )

      (AssociativeLink
         (ConceptNode "want-this")
         (ConceptNode "one-high, 4-arity")
         (ConceptNode "mehh2")
         (ConceptNode "mehh3")
      )

      (AssociativeLink
         (ConceptNode "want-this")
         (MemberLink
            (WordInstanceNode "color")
            (WordInstanceNode "blue")
         )
      )
   )

   (SimilarityLink
      (AssociativeLink
         (ConceptNode "wrong thing")
         (MemberLink
            (WordInstanceNode "color")
            (WordInstanceNode "blue")
         )
      )

      (AssociativeLink
         (FeatureNode "want-this")
         (MemberLink
            (WordInstanceNode "color")
            (WordInstanceNode "blue")
         )
      )
   )

   (AssociativeLink
      (FeatureNode "want-this")
      (FeatureNode "not really")
      (MemberLink
         (WordInstanceNode "color")
         (WordInstanceNode "blue")
      )
   )
)

(AssociativeLink
   (MemberLink
      (WordInstanceNode "color")
      (WordInstanceNode "blue")
   )
   (ConceptNode "want-this")
)

(AssociativeLink
   (ConceptNode "want-this")
   (InheritanceLink
      (WordInstanceNode "color")
      (WordInstanceNode "green")
   )
)

(AssociativeLink
   (ConceptNode "want-this")
   (MemberLink
      (WordInstanceNode "color")
      (WordInstanceNode "blue")
   )
   (ConceptNode "mehh2")
   (ConceptNode "mehh3")
)

(AssociativeLink
   (FeatureNode "want-this")
   (AssociativeLink
      (FeatureNode "want-this")
      (MemberLink
         (FeatureNode "want-this")
         (AssociativeLink
            (WordInstanceNode "Volkerding")
            (WordInstanceNode "fnord")
         )
      )
      (MemberLink
         (WordInstanceNode "Volkerding")
         (WordInstanceNode "fnord")
      )
      (MemberLink
         (WordInstanceNode "Volkerding")
         (WordInstanceNode "fnord")
      )
   )
)

; Match any arity-2 structure of the desired form.
(define (untyped-link-match)
   (CollectionOf
   (QueryLink
      (VariableList
         (VariableNode "$var")
      )
      (AndLink
         (AssociativeLink
            (ConceptNode "want-this")
            (VariableNode "$var")
         )
      )
      (VariableNode "$var")
   )
   )
)

; Match arity-2 with the link having a type.
(define (typed-link-match)
   (CollectionOf
   (QueryLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$var")
            (TypeNode "MemberLink")
         )
      )
      (AndLink
         (AssociativeLink
            (ConceptNode "want-this")
            (VariableNode "$var")
         )
      )
      (VariableNode "$var")
   )
   )
)

; Match any arity-two structure
(define (untyped-any-match)
   (CollectionOf
   (QueryLink
      (VariableList
         (VariableNode "$var-a")
         (VariableNode "$var-b")
      )
      (AndLink
         (AssociativeLink
            (VariableNode "$var-a")
            (VariableNode "$var-b")
         )
      )
      (ListLink
         (VariableNode "$var-a")
         (VariableNode "$var-b")
      )
   )
   )
)

; Match typed arity-two structure
(define (typed-memb-link-match)
   (CollectionOf
   (QueryLink
      (VariableList
         (VariableNode "$var-a")
         (TypedVariableLink
            (VariableNode "$var-b")
            (TypeNode "MemberLink")
         )
      )
      (AndLink
         (AssociativeLink
            (VariableNode "$var-a")
            (VariableNode "$var-b")
         )
      )
      (ListLink
         (VariableNode "$var-a")
         (VariableNode "$var-b")
      )
   )
   )
)

(define (typed-pet-node-match)
   (CollectionOf
   (QueryLink
      (VariableList
         (VariableNode "$var-a")
         (TypedVariableLink
            (VariableNode "$var-b")
            (TypeNode "FeatureNode")
         )
      )
      (AndLink
         (AssociativeLink
            (VariableNode "$var-b")
            (VariableNode "$var-a")
         )
      )
      (ListLink
         (VariableNode "$var-b")
         (VariableNode "$var-a")
      )
   )
   )
)
