;
; filter-float-test.scm -- Verify that various wiki pages don't lie.
; Specifically, that the demo in
; https://wiki.opencog.org/w/PromiseLink
; works as promised.

(use-modules (opencog) (opencog exec))
(use-modules (srfi srfi-1))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "filter-float-test")
(test-begin tname)

(define start-time (car (cog-value->list (cog-execute! (Time)))))
(define end-time (+ start-time 3))

; --------------------
; Simplest FloatValue filtering
(define basic (cog-value->list
	(cog-execute! (Filter (TypeNode 'FloatValue) (Time)))))
(test-assert "cnt-basic" (equal? 1 (length basic)))
(define tbasic (car basic))
(test-assert "low-basic" (< start-time tbasic))
(test-assert "high-basic" (< tbasic end-time))

; --------------------
; Type match fails
(define noval (cog-execute! (Filter (TypeNode 'Value) (Time))))
(test-assert "cnt-noval" (equal? #f noval))

; --------------------
; Inherit FloatValue filtering
(define inherit (cog-value->list
	(cog-execute! (Filter (TypeInhNode 'Value) (Time)))))
(test-assert "cnt-inherit" (equal? 1 (length inherit)))
(define tinherit (car inherit))
(test-assert "low-inherit" (< start-time tinherit))
(test-assert "high-inherit" (< tinherit end-time))

; --------------------
; RuleLink FloatValue filtering
(define ruleuni (cog-value->list
	(cog-execute! (Filter
		(Rule
			(TypedVariable (Variable "$x") (TypeInhNode 'Value))
			(Variable "$x") ; body
			(Plus (Variable "$x") (Number 2)))
		(Time)))))
(test-assert "cnt-ruleuni" (equal? 1 (length ruleuni)))
(define truleuni (- (car ruleuni) 2))
(test-assert "low-ruleuni" (< start-time truleuni))
(test-assert "high-ruleuni" (< truleuni end-time))

; --------------------
; RuleLink Multi-target filtering
(define rulemulti (cog-value->list
	(cog-execute! (Filter
		(Rule
			(TypedVariable (Variable "$x") (TypeNode 'FloatValue))
			(Variable "$x")  ;  body
			(Plus (Variable "$x") (Number -2))
			(Plus (Variable "$x") (Number -1))
			(Variable "$x")
			(Plus (Variable "$x") (Number 1))
			(Plus (Variable "$x") (Number 2)))
		(Time)))))
(test-assert "cnt-rulemulti" (equal? 5 (length rulemulti)))

(define tmulti-0 (+ (cog-value-ref (list-ref rulemulti 0) 0) 2))
(test-assert "low-rulemulti" (< start-time tmulti-0))
(test-assert "high-rulemulti" (< tmulti-0 end-time))

(define tmulti-1 (+ (cog-value-ref (list-ref rulemulti 1) 0) 1))
(test-assert "low-rulemulti" (< start-time tmulti-1))
(test-assert "high-rulemulti" (< tmulti-1 end-time))

(define tmulti-2 (+ (cog-value-ref (list-ref rulemulti 2) 0) 0))
(test-assert "low-rulemulti" (< start-time tmulti-2))
(test-assert "high-rulemulti" (< tmulti-2 end-time))

(define tmulti-3 (+ (cog-value-ref (list-ref rulemulti 3) 0) -1))
(test-assert "low-rulemulti" (< start-time tmulti-3))
(test-assert "high-rulemulti" (< tmulti-3 end-time))

(define tmulti-4 (+ (cog-value-ref (list-ref rulemulti 4) 0) -2))
(test-assert "low-rulemulti" (< start-time tmulti-4))
(test-assert "high-rulemulti" (< tmulti-4 end-time))

; All have the same time exactly (pico-second level)
(define eps 1e-12)
(test-assert "rulemulti-equal-01" (< (abs (- tmulti-1 tmulti-0)) eps))
(test-assert "rulemulti-equal-02" (< (abs (- tmulti-2 tmulti-0)) eps))
(test-assert "rulemulti-equal-03" (< (abs (- tmulti-3 tmulti-0)) eps))
(test-assert "rulemulti-equal-04" (< (abs (- tmulti-4 tmulti-0)) eps))

; --------------------
; Promise Multi-target filtering
(define prom
	(Promise
		(TypeNode 'FutureStream)
		(Filter
			(Rule
				(TypedVariable (Variable "$x") (TypeNode 'FloatValue))
				(Variable "$x")  ;  body
				(Plus (Variable "$x") (Number -2))
				(Plus (Variable "$x") (Number -1))
				(Variable "$x")
				(Plus (Variable "$x") (Number 1))
				(Plus (Variable "$x") (Number 2)))
			(Time))))
(define promset (cog-execute! prom))

(define linkset (cog-value->list promset))
(test-assert "cnt-promise-linkset" (equal? 1 (length linkset)))

(define rulepromise (cog-value->list (list-ref linkset 0)))
(test-assert "cnt-rulepromise" (equal? 5 (length rulepromise)))

(define tpromise-0 (+ (cog-value-ref (list-ref rulepromise 0) 0) 2))
(test-assert "low-rulepromise" (< start-time tpromise-0))
(test-assert "high-rulepromise" (< tpromise-0 end-time))

(define tpromise-1 (+ (cog-value-ref (list-ref rulepromise 1) 0) 1))
(test-assert "low-rulepromise" (< start-time tpromise-1))
(test-assert "high-rulepromise" (< tpromise-1 end-time))

(define tpromise-2 (+ (cog-value-ref (list-ref rulepromise 2) 0) 0))
(test-assert "low-rulepromise" (< start-time tpromise-2))
(test-assert "high-rulepromise" (< tpromise-2 end-time))

(define tpromise-3 (+ (cog-value-ref (list-ref rulepromise 3) 0) -1))
(test-assert "low-rulepromise" (< start-time tpromise-3))
(test-assert "high-rulepromise" (< tpromise-3 end-time))

(define tpromise-4 (+ (cog-value-ref (list-ref rulepromise 4) 0) -2))
(test-assert "low-rulepromise" (< start-time tpromise-4))
(test-assert "high-rulepromise" (< tpromise-4 end-time))

; All have the same time exactly (pico-second level)
; (define eps 1e-12)
(test-assert "rulepromise-equal-01" (< (abs (- tpromise-1 tpromise-0)) eps))
(test-assert "rulepromise-equal-02" (< (abs (- tpromise-2 tpromise-0)) eps))
(test-assert "rulepromise-equal-03" (< (abs (- tpromise-3 tpromise-0)) eps))
(test-assert "rulepromise-equal-04" (< (abs (- tpromise-4 tpromise-0)) eps))

; sleep and try again
(sleep 1)

(define linkset2 (cog-value->list promset))
(test-assert "cnt-promise-linkset2" (equal? 1 (length linkset2)))

(define rulepromise2 (cog-value->list (list-ref linkset2 0)))
(test-assert "cnt-rulepromise2" (equal? 5 (length rulepromise2)))

(define tpromise2-0 (+ (cog-value-ref (list-ref rulepromise2 0) 0) 2))
(test-assert "low-rulepromise2" (< start-time tpromise2-0))
(test-assert "high-rulepromise2" (< tpromise2-0 end-time))

(define tpromise2-1 (+ (cog-value-ref (list-ref rulepromise2 1) 0) 1))
(test-assert "low-rulepromise2" (< start-time tpromise2-1))
(test-assert "high-rulepromise2" (< tpromise2-1 end-time))

(define tpromise2-2 (+ (cog-value-ref (list-ref rulepromise2 2) 0) 0))
(test-assert "low-rulepromise2" (< start-time tpromise2-2))
(test-assert "high-rulepromise2" (< tpromise2-2 end-time))

(define tpromise2-3 (+ (cog-value-ref (list-ref rulepromise2 3) 0) -1))
(test-assert "low-rulepromise2" (< start-time tpromise2-3))
(test-assert "high-rulepromise2" (< tpromise2-3 end-time))

(define tpromise2-4 (+ (cog-value-ref (list-ref rulepromise2 4) 0) -2))
(test-assert "low-rulepromise2" (< start-time tpromise2-4))
(test-assert "high-rulepromise2" (< tpromise2-4 end-time))

; All have the same time exactly (pico-second level)
; (define eps 1e-12)
(test-assert "rulepromise2-equal-01" (< (abs (- tpromise2-1 tpromise2-0)) eps))
(test-assert "rulepromise2-equal-02" (< (abs (- tpromise2-2 tpromise2-0)) eps))
(test-assert "rulepromise2-equal-03" (< (abs (- tpromise2-3 tpromise2-0)) eps))
(test-assert "rulepromise2-equal-04" (< (abs (- tpromise2-4 tpromise2-0)) eps))


(test-end tname)

(opencog-test-end)
