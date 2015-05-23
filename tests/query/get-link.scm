;
; Data and tests for GetLink
;

(use-modules ((opencog query)))


(InheritanceLink (ConceptNode "Ben") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Linas") (ConceptNode "human"))

(define is-human 
	(GetLink (InheritanceLink (VariableNode "$H") (ConceptNode "human"))))
