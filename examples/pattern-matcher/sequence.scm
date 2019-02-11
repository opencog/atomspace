;
; sequence.scm -- Behavior sequences
;
; Demonstrate using behavior trees to interact with external systems,
; such as robots or game worlds.  Behavior trees (see Wikipedia:
; https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)
; are commonly used in game worlds to control non-player characters
; and to perform AI stimulus-response action sequences ("SRAI").
;
; The SatisfactionLink, combined with the SequentialAndLink, implements
; the concept of a behavior tree "sequence node", and so writing
; behavior trees in Atomese is straight-forward. (See also ParallelLink
; and JoinLink for general scripting in multiple threads).
;
; Interacting with external systems in Atomese can be accomplished with
; GroundedPredicateNode and with GroundedSchemaNode. This example
; demonstrates how behavior scripting can be used to obtain input from
; external systems.
;
; This example uses the GroundedPredicateNode to provide a simple
; behavior sequence: i.e. a set of steps that are conditionally played
; out, depending on whether the predicate returns true of false.
; There are no variables in this demo; thus, the sequence is not
; a graph search, but is rather a straight-forward if-else sequence
; evaluation.
;
; This demo also demonstrates the use of a SatisfactionLink: there is no
; output, and no graph-rewriting occurs, thus a BindLink is not needed,
; and the simpler SatisfactionLink is enough.
;
;
(use-modules (opencog) (opencog exec))

(define green-light  (Concept "green light"))
(define red-light  (Concept "red light"))

; Counts of how many times the green and red lights were seen.
(define num-green 0)
(define num-red 0)

; Return SimpleTruthValue of true if green light, false if red light,
; and throw an exception if neither.  Increment counters so that we
; can verify that this was invoked.
(define (stop-go atom)
	(format #t "Got called with this: ~A\n" (cog-name atom))
	(cond
		((equal? atom green-light) (begin (set! num-green (+ 1 num-green)) (stv 1 1)))
		((equal? atom red-light) (begin (set! num-red (+ 1 num-red)) (stv 0 1)))
		(else (throw 'not-a-stoplight "stop-go" "you're busted"))
	)
)

; Should throw an exception in all cases. Shouldn't do donuts on
; corn fields.
(define off-road
	(Satisfaction
		(VariableList)  ; no variables
		(SequentialAnd
			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List (Concept "corn field"))))))

;; Should see two green lights, and one red light, after which
;; the matching should stop.  There should be no exceptions or
;; errors when evaluating this.
(define traffic-lights
	(Satisfaction
		(VariableList)  ; no variables
		(SequentialAnd
			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List green-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List green-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List red-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List (Concept "traffic ticket"))))))

(define (start-again)
	(cog-evaluate! traffic-lights)
	(format #t "Have seen ~A green lights and ~A  red lights\n"
		num-green num-red))

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
	(Satisfaction
		(VariableList)  ; no variables
		(SequentialOr   ; <==== unlike before, this is OR
			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List red-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List red-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List red-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List green-light))

			(Evaluation
				(GroundedPredicateNode "scm: stop-go")
				(List (Concept ".... And they're off!"))))))

(define (drag-race)
	(cog-evaluate! hot-rodding)
	(simple-format #t "Waited on ~A red lights\n" num-red))

;;; The below should result in the three red lights before it turns
;;; green.
;;;
; (drag-race)
; (drag-race)
; (drag-race)
