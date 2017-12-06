;
; scope-quote.scm
;
; The current ForwardChainerUTest creates some bindlinks that are
; very similar to this. These search for a combination of AndLinks
; and LambdaLinks, and hope to match the variable names that appear
; inside of them.  The extra quoting places some extra pressures
; and complexity on handling the ScopeLinks correctly. This tests
; that.
;
; There are two variants in here: one with an AndLink, and one
; without.  The issue is that the AndLink has a special meaning for
; the BindLink, as opposed to PLN, and thus must be quoted. Also,
; it is an unordered link, so takes a different path through the
; pattern matcher.  Thus, the second variant uses a ListLink instead,
; to simplify debugging, in case there is a bug.

(use-modules (opencog))
(use-modules (opencog exec))


; Sample data
(define p-lamb
  (LambdaLink
    (TypedVariableLink
      (VariableNode "$Xaaaa")
      (TypeNode "ConceptNode"))
    (PredicateNode "P")))

(define q-lamb
  (LambdaLink
    (TypedVariableLink
      (VariableNode "$Xbee")
      (TypeNode "ConceptNode"))
    (EvaluationLink
      (PredicateNode "Q")
      (VariableNode "$Xbee"))))

; The ListLink variant
(ListLink p-lamb q-lamb)

; The AndLink variant -- same as above, except uses AndLink.
(AndLink p-lamb q-lamb)

(define A1-lamb
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableNode "$TyVs-one"))
      (UnquoteLink
        (VariableNode "$A1")))))

(define A2-lamb
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableNode "$TyVs-two"))
      (UnquoteLink
        (VariableNode "$A2")))))

; The pattern matcher, looks for the List variant
(define blist
  (BindLink
    (VariableList
      (TypedVariableLink
        (VariableNode "$TyVs-one")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (TypedVariableLink
        (VariableNode "$TyVs-two")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (VariableNode "$A1")
      (VariableNode "$A2")
    )
    ; pattern
    (ListLink
      A1-lamb
      A2-lamb)
    ; result
    (OrderedLink
      A1-lamb
      A2-lamb))
)

; The pattern matcher, looks for the And variant. Notice that
; the quoting is different from the above.
(define bland
  (BindLink
    ; Variable declaration
    (VariableList
      (TypedVariableLink
        (VariableNode "$TyVs-one")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (TypedVariableLink
        (VariableNode "$TyVs-two")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (VariableNode "$A1")
      (VariableNode "$A2")
    )
    ; pattern to match
    (QuoteLink
      (AndLink
        (LambdaLink
          (UnquoteLink
            (VariableNode "$TyVs-one"))
          (UnquoteLink
            (VariableNode "$A1")))
        (LambdaLink
          (UnquoteLink
            (VariableNode "$TyVs-two"))
          (UnquoteLink
            (VariableNode "$A2")))))
    ; output result
    (UnorderedLink
      A1-lamb
      A2-lamb))
)
