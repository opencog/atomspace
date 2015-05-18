
(use-modules (opencog))
(use-modules (opencog query))

(define crasher
	(BindLink
   	(ImplicationLink
      	(VariableNode "$x")
      	(ListLink
         	(ConceptNode "And the answer is ...")
         	(VariableNode "$x")
         	(QuoteLink (VariableNode "$x"))))))
