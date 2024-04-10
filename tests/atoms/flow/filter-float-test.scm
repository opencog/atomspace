;
; filter-float-test.scm -- Verify that various wiki pages don't lie.
;

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
(test-assert "cnt-rulemulti" (equal? 1 (length rulemulti)))
(define trulemulti (- (car rulemulti) 2))
(test-assert "low-rulemulti" (< start-time trulemulti))
(test-assert "high-rulemulti" (< trulemulti end-time))

(test-end tname)

(opencog-test-end)
