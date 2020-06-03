;; Unit test for issue #2382
;;
;; An atom is created in some auxiliary atomspace, then that atom is
;; used to access such auxiliary atomspace in some peculiar fashion,
;; and the created atom becomes invalid as a result.

(use-modules (opencog))
(use-modules (opencog logger))
(cog-logger-set-timestamp! #f)
(cog-logger-set-stdout! #t)
(cog-logger-set-sync! #t)

;; Save initial atomspace
(define initial-as (cog-atomspace))

;; Define an auxiliary atomspace
(define auxiliary-as (cog-new-atomspace))

;; Switch to the auxiliary atomspace
(define pre-pre-as (cog-set-atomspace! auxiliary-as))

;; Inside this auxiliary atomspace, create concept A
(define auxiliary-A (Concept "A"))

;; Switch the atomspace where A is (which is auxiliary-as)
(define pre-as (cog-set-atomspace! (cog-atomspace auxiliary-A)))

;; Verify that the current atomspace, auxiliary-as and the prevous one
;; are all the same
(cog-logger-info "[1] current as = ~a" (cog-atomspace))
(cog-logger-info "[1] auxiliary-as = ~a" auxiliary-as)
(cog-logger-info "[1] pre-as = ~a" pre-as)

;; Come back to the previous atomspace (which should still be auxiliary-as)
(cog-set-atomspace! pre-as)

;; Verify that the current atomspace and auxiliary-as are the same
(cog-logger-info "[2] current as = ~a" (cog-atomspace))
(cog-logger-info "[2] auxiliary-as = ~a" auxiliary-as)

;; Come back to the previous of the previous atomspace (which should be the initial atomspace)
(cog-set-atomspace! pre-pre-as)

;; Verify that the current atomspace and initial-as are the same
(cog-logger-info "[3] current as = ~a" (cog-atomspace))
(cog-logger-info "[3] initial-as = ~a" initial-as)

;; Switch again to the auxiliary atomspace
(define pre-as-again (cog-set-atomspace! auxiliary-as))

;; Create again concept A in it
(define A (Concept "A"))

;; Switch back to initial atomspace
(cog-set-atomspace! pre-as-again)

;; Call Garbage collector and sleep for a second to give it enough
;; time to (wrongly) release the auxiliary atomspace
(gc)
(sleep 1)

;; Now A is an invalid handle.
;;
;; Very strangly if the line
;;
;; (define pre-as (cog-set-atomspace! (cog-atomspace auxiliary-A)))
;;
;; is replaced by
;;
;; (define pre-as (cog-set-atomspace! auxiliary-as))
;;
;; which should be equivalent, then A is no longer invalid.

;; The atomspace of A should be the auxiliary atomspace
(define A-as (cog-atomspace A))
