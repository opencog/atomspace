;
; sequence.scm
;
; Demonstrate using the GroundedPredicateNode to provide a simple
; behavior sequence: i.e. a set of steps that are conditionally played
; out, depending on whether the predicate node returns true of false.
; There are no variables in this demo; thus, the sequence is not
; a graph search, but is rather a straight-forward if-else sequence
; evaluation.
;
; This demo also demonstrates the use of a SatisfactionLink: there is no
; output, and no graph-rewriting occurs, thus a BindLink is not needed,
; and the simpler SatisfactionLink is enough.
;
; The SatisfactionLink, combined with the SequentialAndLink, implement
; the concept of a behavior tree "sequence node". See
; https://en.wikipedia.org/wiki/Behavior_Trees_(artificial_intelligence,_robotics_and_control)
;
; The run this, you might need to do this:
;
; OCDIR=/home/yourname/where/you/put/opencog
; export LTDL_LIBRARY_PATH=$OCDIR/build/opencog/guile:$OCDIR/build/opencog/query
;
; Add the following to your ~/.guile file:
; (add-to-load-path "/home/yourname/opencog/build")
; (add-to-load-path "/home/yourname/opencog/opencog/scm")
; (add-to-load-path ".")
;
; Start guile:
; guile
;
; and then load this file:
; (load-from-path "sequence.scm")
;
; Then, scroll to the bottom, and try some of the commented-out
; examples.
;
(use-modules (opencog) (opencog query) (opencog exec))

(define green-light  (ConceptNode "green light"))
(define red-light  (ConceptNode "red light"))

; Counts of how many times the green and red lights were seen.
(define num-green 0)
(define num-red 0)

; Return SimpleTV of true if green light, false if red light, and
; throw an exception if neither.  Increment counters so that we
; can verify that this was invoked.
(define (stop-go atom)
	(cond
		((equal? atom green-light) (begin (set! num-green (+ 1 num-green)) (stv 1 1)))
		((equal? atom red-light) (begin (set! num-red (+ 1 num-red)) (stv 0 1)))
		(else (throw 'not-a-stoplight "stop-go" "you're busted"))
	)
)

; Should throw an exception in all cases. Shouldn't do donuts on
; corn fields.
(define off-road
	(SatisfactionLink
		(VariableList)  ; no variables
		(SequentialAndLink
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink
					(ConceptNode "corn field")
				)
			)
		)
	)
)

;; Should see two green lights, and one red light, after which
;; the matching should stop.  There should be no exceptions or
;; errors when evaluating this.
(define traffic-lights
	(SatisfactionLink
		(VariableList)  ; no variables
		(SequentialAndLink
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink green-light)
			)

			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink green-light)
			)

			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)

			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink
					(ConceptNode "traffic ticket")
				)
			)
		)
	)
)

(define (start-again)
	(cog-evaluate! traffic-lights)
	(simple-format #t "Went through ~A green lights and ~A  red lights\n"
		num-green num-red)
)

;;; Try the below.  This should result in an exception being thrown.
;;;
; (cog-evaluate! off-road)
;
;;; The below should result in the green light being seen twice, and
;;; the red light once, and no exceptions or errors.
;;;
; (start-again)
; (start-again)
; (start-again)
;
;
;;; The below is very similar to the above, except that this uses
;;; a SequentialOrLink that halts after the first TRUE value.
;;;
(define hot-rodding
	(SatisfactionLink
		(VariableList)  ; no variables
		(SequentialOrLink   ; <==== unlike before, this it OR
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink green-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink
					(ConceptNode ".... And they're off!")
				)
			)
		)
	)
)

(define (drag-race)
	(cog-evaluate! hot-rodding)
	(simple-format #t "Waited on ~A red lights\n" num-red)
)

;;; The below should result in the three red lights before it turns
;;; green.
;;;
; (drag-race)
; (drag-race)
; (drag-race)
