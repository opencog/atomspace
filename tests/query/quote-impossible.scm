;
; quote-impossible.scm
;
; Currently triggers bug #1531 "Impossible situation"
; The problem is that the search tries to start within
; the QuoteLink, but do_term_up does not like that.
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
