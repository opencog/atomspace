;
; futures.scm
;
; Assorted dynamic API's. Starting with MI.
;
(define-public (add-dynamic-mi LLOBJ)

	(define mi-key (Predicate "*-Dynamic MI Key-*"))
	(define cnt-key (LLOBJ 'count-key))
	(define cnt-ref (LLOBJ 'count-ref))

	(define (make-forumla)
		(define lrp (LLOBJ 'make-pair (Variable "$L") (Variable "$R")))
		(define lwp (LLOBJ 'left-wildcard (Variable "$R")))
		(define rwp (LLOBJ 'right-wildcard (Variable "$L")))
		(define wwp (LLOBJ 'wild-wild))
		(DefineLink
			(DefinedProcedure "dynamic MI")
			(Lambda
				(VariableList (Variable "$L") (Variable "$R"))
				(Log2
					(Divide
						(Times
							(FloatValueOf lrp cnt-key)
							(FloatValueOf wwp cnt-key))
						(Times
							(FloatValueOf lwp cnt-key)
							(FloatValueOf rwp cnt-key))))))


