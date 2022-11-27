;
; futures.scm
;
; Assorted dynamic API's. Starting with MI.
;

(use-modules (srfi srfi-1))

(define-public (add-dynamic-mi LLOBJ)
"
  add-dynamic-mi LLOBJ -- Add formula to dynamically recompute the MI

  Whenever a pair is references, the MI for that pair is recomputed for
  the count values on that pair.  Uses the conventional asymmetric
  formula.
"
	; Check for valid strructure
	(if (not (and (LLOBJ 'provides 'count-key) (LLOBJ 'provides 'count-ref)))
		(throw 'wrong-type-arg 'add-dynamic-mi
			"Expecting a count object, to access the raw counts!"))

	; Location where the MI will be stored.
	(define mi-key (Predicate "*-Dynamic MI Key-*"))

	; Name of function that will compute it.
	; It needs to be globally-unique! So append the object ID
	(define dyn-proc
		(DefinedProcedure (string-append "*-dynamic MI " (LLOBJ 'id))))

	(define (make-formula)
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
							(FetchValueOf lrp cnt-key cnt-ref)
							(FetchValueOf wwp cnt-key cnt-ref))
						(Times
							(FetchValueOf lwp cnt-key cnt-ref)
							(FetchValueOf rwp cnt-key cnt-ref)))))))

	(LLOBJ 'get-count (LLOBJ 'wild-wild))
	(make-formula)

	; Install the formula for this pair.
	(define (install-formula PAIR L R)
		; Touch all of the counts, at least once, in case they
		; are sitting storage. Actually, no; FetchValueOf will
		; do that for us.
		; (LLOBJ 'pair-count L R)
		; (LLOBJ 'get-count (LLOBJ 'right-wildcard L))
		; (LLOBJ 'get-count (LLOBJ 'left-wildcard R))
		(cog-set-value! PAIR mi-key (FormulaStream
			(ExecutionOutput dyn-proc (List L R)))))

	; Get the MI for this pair. Install the formula, if not yet
	; installed.
	(define (get-mi PAIR L R)
		(define miv (cog-value PAIR mi-key))
		(when (not miv)
			(install-formula PAIR L R)
			(set! miv (cog-value PAIR mi-key)))
		(cog-value-ref miv 0))

	; Get the MI, given only the pair.
	(define (get-count PAIR)
		(get-mi PAIR (LLOBJ 'left-element PAIR) (LLOBJ 'right-element PAIR)))

	; Get the MI, given only the left and right bits.
	(define (pair-count L-ATOM R-ATOM)
		; Force LLOBJ to fetch from storage.
		(LLOBJ 'pair-count L-ATOM R-ATOM)
		; If it was found, then compute the MI. Else not.
		(define stats-atom (LLOBJ 'get-pair L-ATOM R-ATOM))
		(if (nil? stats-atom) -inf.0
			(get-mi stats-atom L-ATOM R-ATOM)))

	(define (help)
		(format #t
			(string-append
"This is the `add-dynamic-mi` object applied to the \"~A\"\n"
"object.  It installs a formula that dynamically computes the MI\n"
"for each pair.  For more information, say `,d add-dynamic-mi` or\n"
"`,describe add-dynamic-mi` at the guile prompt, or just use the\n"
"'describe method on this object. You can also get at the base\n"
"object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-dynamic-mi 'documentation)))

	;-------------------------------------------
	; Explain what is provided.
	(define (count-type) 'FloatValue)
	(define (count-key)  mi-key)
	(define (count-ref)  0)
	(define (provides meth)
		(case meth
			((count-type)    count-type)
			((count-key)     count-key)
			((count-ref)     count-ref)
			((pair-count)    pair-count)
			((get-count)     get-count)
			(else            (LLOBJ 'provides meth))))

	;-------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((count-type)       (count-type))
			((count-key)        (count-key))
			((count-ref)        (count-ref))
			((pair-count)       (apply pair-count args))
			((get-count)        (apply get-count args))

			((formula)          dyn-proc)

			((provides)         (apply provides args))
			((help)             (help))
			((describe)         (describe))
			((obj)              "add-dnymaic-mi")
			((base)             LLOBJ)
			(else               (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------
; Example usage
;
; (define ala (make-any-link-api))
; (define alc (add-count-api ala))
; (define als (add-storage-count alc))
; (define ady (add-dynamic-mi als))
; (ady 'pair-count (Word "the") (Word "horse"))
