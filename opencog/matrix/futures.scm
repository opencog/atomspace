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
		(define cnt-ref (LLOBJ 'count-ref))

		; Create a mask to fish out the on single number, by decimation.
		; For example, for ref=2, the mask is "0 0 1"
		(define zero-pad (string-join (make-list cnt-ref "0")))
		(define cnt-mask (Number (string-append zero-pad " 1")))

		; Create the actual procedure
		(DefineLink
			dyn-proc
			(Lambda
				(VariableList (Variable "$L") (Variable "$R"))
				(Log2
					(Divide
						(Times
							(Decimate cnt-mask (FloatValueOf lrp cnt-key))
							(Decimate cnt-mask (FloatValueOf wwp cnt-key)))
						(Times
							(Decimate cnt-mask (FloatValueOf lwp cnt-key)
							(Decimate cnt-mask (FloatValueOf rwp cnt-key))))))))
	)


