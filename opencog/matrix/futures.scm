;
; futures.scm
;
; Assorted dynamic API's. Starting with MI.
;

(use-modules (srfi srfi-1))

(define-public (add-dynamic-mi LLOBJ)

	; Check for valid strructure
	(if (or (not (LLOBJ 'provides 'count-key) (LLOBJ 'provides 'count-ref)))
		(throw 'wrong-type-arg 'add-dynamic-mi
			"Expecting a count object, to access the raw counts!"))

	; Location where the MI will be stored.
	(define mi-key (Predicate "*-Dynamic MI Key-*"))

	; Name of function that will compute it.
	; It needs to be globally-unique! So append the object ID
	(define dyn-proc
		(DefinedProcedure (string-append "*-dynamic MI " (LLOBJ 'id))))

	(define (make-forumla)
		; The various pairs, per the LLOBJ
		(define lrp (LLOBJ 'make-pair (Variable "$L") (Variable "$R")))
		(define lwp (LLOBJ 'left-wildcard (Variable "$R")))
		(define rwp (LLOBJ 'right-wildcard (Variable "$L")))
		(define wwp (LLOBJ 'wild-wild))

		; The location of the count, per the LLOBJ, which had
		; better be a count-obj, or else!
		(define cnt-key (LLOBJ 'count-key))
		(define cnt-ref (NumberNode (LLOBJ 'count-ref)))

		; Create the actual procedure
		(DefineLink
			dyn-proc
			(Lambda
				(VariableList (Variable "$L") (Variable "$R"))
				(Log2
					(Divide
						(Times
							(FloatValueOf lrp cnt-key cnt-ref)
							(FloatValueOf wwp cnt-key cnt-ref))
						(Times
							(FloatValueOf lwp cnt-key cnt-ref
							(FloatValueOf rwp cnt-key cnt-ref)))))))
	)


