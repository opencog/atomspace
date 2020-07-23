;
; queue.scm
;
; Example of using QueueValues to communicate results from one thread
; to another.
;
; XXX FIXME, this example is not yet complete and does not yet work...
;
(use-modules (opencog) (opencog exec))

; QueueValues are much like LinkValues, in that they can hold sequences
; of values. Here's a static sequence of values.
(cog-set-value!
	(Concept "Aybe Sea") (Predicate "key")
	(QueueValue 
		(Concept "E") (Concept "A") (Concept "D")
		(Concept "G") (Concept "B") (Concept "E")))

(cog-execute! (StreamValueOf (Concept "Aybe Sea") (Predicate "key")))

; Generate some ConceptNodes...
(define generator
	(let ((str "a"))
		(lambda () (set! str (string-concatenate (list str "b")))
			(Concept str))))

(define (create) (generator))

(define (prt atom)
	(format #t "Got this atom: ~A\n" atom)
	(stv 1 1))

; An atom with a queue
(cog-set-value!  (Concept "abc") (Predicate "key") (QueueValue))

; Producer, consumer threads
; XXX This is incorrect, because instead of calling SetValue,
; we really need to by queueing (pushing) the results onto the
; queue.
(define mt
	(ThreadJoin
		(SequentialAnd
			(True (SetValue (Concept "abc") (Predicate "key")
				(ExecutionOutput (GroundedSchema "scm: create") (List))))
			(True (Sleep (Number 1)))
			(True (SetValue (Concept "abc") (Predicate "key")
				(ExecutionOutput (GroundedSchema "scm: create") (List))))
			(True (Sleep (Number 1)))
			(True (SetValue (Concept "abc") (Predicate "key")
				(ExecutionOutput (GroundedSchema "scm: create") (List))))
			(True (Sleep (Number 1)))
			(True (SetValue (Concept "abc") (Predicate "key")
				(ExecutionOutput (GroundedSchema "scm: create") (List))))
		)
		(SequentialAnd
			(Evaluation (GroundedPredicate "scm: prt")
				(List (StreamValueOf (Concept "abc") (Predicate "key"))))
	)))

; (cog-execute! mt)
