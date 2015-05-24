;
; Guile assert/retract example.
;
; The cog-execute! function is used to assert facts, or retract them
; from the AtomSpace.  The idea of asserting and retracting facts is
; taken from ProLog, where the system as a whole behave like a database,
; and there must be a way of adding records, or removing them from the
; database.  So, likewise, in the AtomSpace: the AtomSpace is a database,
; and the InsertLink and RemoveLink provide a way to add and remove
; statements when they are executed.
;
(use-modules (opencog))

; A utility function to print all EvaluationLinks in the AtomSpace.
(define (show-eval-links)
	(cog-map-type (lambda (h) (display h) #f) 'EvaluationLink))

; The EvaluationLink won't be added until this is reduced.
; When it is reduced, the ListLink will be substitited for the
; variable $x, creating the fully-assembled EvaluationLink.
(define to-be-added
	(PutLink
		(EvaluationLink
		    (PredicateNode "some property")
          (VariableNode "$x"))
		(ListLink
			(ConceptNode "thing A")
			(ConceptNode "B-dom-ness"))))

; Verify that the atomspace contains no EvaluationLinks:
(show-eval-links)

; Now, actually create the EvaluationLink
(cog-reduce! to-be-added)

; Take a look again:
(show-eval-links)

; The PutLink below causes the put-link above to be un-done.
; It explicitly specifies the same parts as were specified above,
; but when these are assembled, it causes the DeleteLink to
; run and remove them.
(define remove-thing-ab
	(PutLink
		(DeleteLink
			(EvaluationLink
				(PredicateNode "some property")
				(VariableNode "$x")))
		(ListLink
			(ConceptNode "thing A")
			(ConceptNode "B-dom-ness"))))

(cog-reduce! remove-thing-ab)

(define to-be-removed
	(PutLink
		(DeleteLink
			(EvaluationLink
				(PredicateNode "some property")
				(VariableNode "$x")))
		(GetLink
			(EvaluationLink
				(PredicateNode "some property")
				(VariableNode "$x")))))

; Now, remove the EvaluationLink
(cog-execute! to-be-removed)
(show-eval-links)

; We can now add and remove over and over:
(cog-execute! to-be-added)
(show-eval-links)

(cog-execute! to-be-removed)
(show-eval-links)

(cog-execute! to-be-added)
(show-eval-links)
(cog-execute! to-be-removed)
(show-eval-links)

; ------------------------------------------------
; The AssignLink combines the add and remove into one.
(define assign-b
	(AssignLink
		(TypeNode "EvaluationLink")
		(PredicateNode "some property")
		(ListLink
			(ConceptNode "thing A")
			(ConceptNode "alternative B"))))

(define assign-v
	(AssignLink
		(TypeNode "EvaluationLink")
		(PredicateNode "some property")
		(ListLink
			(ConceptNode "thing A")
			(ConceptNode "The V alternative"))))

(cog-execute! assign-b)
(show-eval-links)

(cog-execute! assign-v)
(show-eval-links)

(cog-execute! assign-b)
(show-eval-links)

; ... and so on, ad infinitum
