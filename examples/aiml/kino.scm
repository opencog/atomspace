
(define (mapConceptualizeString input)
	(ListLink 
		(map (lambda (x) (ConceptNode x )) (string-split input #\ )))
	)

; (mapConceptualizeString "I love you")

(define (genQueryPattern input)
	(PatternLink
		(BindLink
			(mapConceptualizeString input)
			(VariableNode "$impl2")
		)))

(BindLink
	(ListLink
		(ConceptNode "I")
		(VariableNode "$star")
		(ConceptNode "you"))
	(ListLink
		(ConceptNode "I")
		(VariableNode "$star")
		(ConceptNode "you")
		(ConceptNode "too")))

(BindLink
	(ListLink
		(ConceptNode "I")
		(ConceptNode "love")
		(VariableNode "$star"))
	(ListLink
		(ConceptNode "I")
		(ConceptNode "like")
		(VariableNode "$star")
		(ConceptNode "a")
		(ConceptNode "lot!")))


(define (findQueryPatterns input)
	(cog-recognize (genQueryPattern input)))

(define (generateReply input)
  (map cog-bind (cog-outgoing-set (findQueryPatterns input))))

(define atomspace-stack '())
(define (push-atomspace)
	(set! atomspace-stack (cons (cog-atomspace) atomspace-stack))
	(cog-set-atomspace! (cog-new-atomspace (cog-atomspace))))

(define (pop-atomspace)
	(if (not (null-list? atomspace-stack))
		(begin
			(cog-set-atomspace! (car atomspace-stack))
			(set! atomspace-stack (cdr atomspace-stack))
			) ; (gc) (gc) (gc)) ; MUST GC OR ELSE DELETED ATOMSPACE STICKS AROUND!!
		(throw 'badpop "More pops than pushes!")))


; (push-atomspace)
; (generateReply "I love you")
; (pop-atomspace)
; (push-atomspace)
; (generateReply "I want you")
; (pop-atomspace)
; (push-atomspace)
; (generateReply "I need you")
; (pop-atomspace)


; (cog-prt-atomspace)
; (cog-incoming-set (ConceptNode "love"))
; (cog-incoming-set (ConceptNode "too"))

; cog-atomspace-uuid
; cog-atomspace-clear
; cog-as

; (cog-atomspace-uuid (cog-as (car (cog-incoming-set (ConceptNode "love")))))
; shows 16 .. why is it not deleted ? Oh, its nnot garbage colected!

; (cog-atomspace-uuid (cog-as (car (cog-incoming-set (ConceptNode "too")))))
