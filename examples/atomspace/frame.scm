;
; frame.scm -- Combining StateLink with derived AtomSpaces
;
; The StateLink guarantees that only one copy of the StateLink
; will be present in the AtomSpace.  The cog-push-atomspace and
; cog-pop-atomspace maintain a stack of atomspaces, with each
; derived AtomSpace on the stack shadowing the underlying space.
;
; It should be possible to set the StateLink in a derived AtomSpace
; without affecting the state in the base AtomSpace. That's what is
; demonstrated here.
;
; The word `frame` is used as the title of the example, as the usage
; resembles that of Kripke frames used in forward-chaining.

(use-modules (opencog))

; Record the current atomspace, for later use.
(define root-as (cog-atomspace))

; Create a robot arm that is not holding anything.
(State (Concept "robot arm") (Concept "empty"))

; Print everything, just to check.
(cog-prt-atomspace)

; Create a new AtomSpace
(cog-push-atomspace)

; Print everything. There should be no change.
(cog-prt-atomspace)

; Change the state of the robot arm.
(State (Concept "robot arm")
	(Evaluation (Predicate "holding") (Concept "block 42")))

; Print everything. The state should have changed.
(cog-prt-atomspace)

; Another way of printing the state.
(cog-map-type (lambda (A) (format #t "This is the state: ~A\n" A) #f) 'State)

; Print the state in the root atomspace
(cog-prt-atomspace root-as)

(cog-map-type (lambda (A) (format #t "Before, it was: ~A\n" A) #f)
	'State root-as)

; Reset back to the original AtomSpace.
(cog-pop-atomspace)

; And print, again, to make sure it went back to the original state.
(cog-prt-atomspace)

; The above can also be repeated using `cog-new-atomspace` instead.
; Up to you to explore that!
; That's all for now -- the end!
