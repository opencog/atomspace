
; Test single-letter names; this was buggy.
(Evaluation (Predicate "P") (List (Concept "A") (Concept "B")))

; (Concept "B" (stv 1 1))

; Test UTF-8
(ListLink(ConceptNode "тестирование кода приводит к успеху")




  		(ListLink



(ConceptNode "Codeprüfung führt zum Erfolg")(ConceptNode "'''''''''1;")))

; Test lines that wrap strangely
(MemberLink
(LexicalNode "AGXT")
(ConceptNode "SMP0000055")
)(MemberLink
(LexicalNode "Uniprot:P21549")
(ConceptNode "SMP0000055")
)(EvaluationLink
(PredicateNode "has_name");test comment at end of line
(ListLink
(ConceptNode "SMP0000055");;;;; here too.
(ConceptNode "Alanine Metabolism")))		

;gaf-version: 2.1
  
;

			
(MemberLink
	(LexicalNode "APOC4-APOC2")
	(ConceptNode "GO:0006629"))
(EvaluationLink
	 (PredicateNode "has_name")
	 (ListLink
		 (LexicalNode "APOC4-APOC2")
		 (ConceptNode "Apolipoprotein C-II isoform 1")
	 )
)
                      
(MemberLink
	(LexicalNode "APOC4-APOC2")            
	(ConceptNode "GO:0006869"))			

