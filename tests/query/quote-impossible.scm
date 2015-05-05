;
; quote-impossible.scm
;
; Currently triggers bug #1531 "Impossible situation"
;
(define imp
  (SatisfactionLink
    (VariableNode "$A")
    (AndLink
      (VariableNode "$A")
      (ImplicationLink
        (VariableNode "$A")
        (QuoteLink
          (InheritanceLink
            (VariableNode "$x") 
            (ConceptNode "criminal")))))))

(define (sat-imp) (cog-satisfy x))
