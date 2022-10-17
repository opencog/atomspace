;
; recover-stack-test.scm
;
; Verify that frames can be stacked deeply,
; including proper atom deletion during the progress.
; This test is nearly identical to the
; frame-progressive-test.scm in the atomspace-rocks repo.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(define (get-val ATOM NAME) (inexact->exact
	(cog-value-ref (cog-value ATOM (Predicate NAME)) 2)))

(opencog-test-runner)

; -------------------------------------------------------------------
; Common setup, used by all tests.

;; Modify and store some atoms.
;; These atoms are in COW spaces, and thus, in order for the store
;; to work correctly, we must store the COW atom, returned by the
;; value setter.  This is unintuitive to the casual user!
(define (make-atoms N)
	(define x (Concept "foo"))
	(cog-set-value! x (Predicate "gee") (ctv 1 0 N))

	(define y (Concept "bar"))
	(cog-set-value! y (Predicate "gosh") (ctv 1 0 (+ 1 N)))

	(define z (List x y))
	(cog-set-value! z (Predicate "bang") (ctv 1 0 (+ 2 N)))

	; A different code-branch is taken, when a link contains other links.
	(define w (List z x))
	(cog-set-value! w (Predicate "bash") (ctv 1 0 (+ 3 N)))
)

; Recursive calls to above
(define (recompute N NLOOP)
	(when (< 0 NLOOP)
		(make-atoms N)
		(cog-set-atomspace! (cog-new-atomspace (cog-atomspace)))
		(cog-extract-recursive! (Concept "bar"))
		(cog-set-atomspace! (cog-new-atomspace (cog-atomspace)))
		(recompute (+ N 3) (- NLOOP 1)))
)

(define (progressive-store N)
	(recompute 1 N)
)

; Verify expected contents
(define (progressive-check N)

	; In the top space, foo should be present, but bar and link absent.
	(define x (cog-node 'Concept "foo"))
	(define y (cog-node 'Concept "bar"))
	(test-assert "foo-present" (cog-atom? x))
	(test-assert "bar-absent" (not (cog-atom? y)))

	(test-equal "foo-tv" (+ (* 3 N) 1) (get-val x "gee"))

	(define z (cog-link 'List (Concept "foo") (Concept "bar")))
	(test-assert "link-absent" (not (cog-atom? z)))

	(define w (cog-link 'List (List (Concept "foo") (Concept "bar"))
	                          (Concept "foo")))
	(test-assert "l2-absent" (not (cog-atom? w)))

	; Next one down should have all four atoms
	(define downli (cog-atomspace-env))
	(test-equal "num-childs" 1 (length downli))
	(cog-set-atomspace! (car downli))

	(define x2 (cog-node 'Concept "foo"))
	(define y2 (cog-node 'Concept "bar"))
	(test-equal "foo-as-before" x x2)
	(test-assert "bar-present" (cog-atom? y2))
	(test-equal "bar-tv" (+ (* 3 N) 2) (get-val y2 "gosh"))

	(define z2 (cog-link 'List x2 y2))
	(test-assert "link-present" (cog-atom? z2))
	(test-equal "link-tv" (+ (* 3 N) 3) (get-val z2 "bang"))

	(define w2 (cog-link 'List z2 x2))
	(test-assert "link-present" (cog-atom? w2))
	(test-equal "l2-tv" (+ (* 3 N) 4) (get-val w2 "bash"))

	; Recurse downwards
	(define downext (cog-atomspace-env))
	(when (equal? 1 (length downext))
		(cog-set-atomspace! (car downext))
		(progressive-check (- N 1)))
)

; ===================================================================

; Test that progressive changes work correctly.
(define (test-progressive)

	; Number of AtomSpaces to create.
	(define STACK-DEPTH 1500)

	; Write a bunch of atoms
	(progressive-store STACK-DEPTH)

	; Check the result, recursing downwards.
	(cog-set-atomspace! (car (cog-outgoing-set (cog-atomspace))))
	(progressive-check (- STACK-DEPTH 1))
)

(define progressive-work "test progressive work")
(test-begin progressive-work)
(test-progressive)
(test-end progressive-work)

; ===================================================================
(opencog-test-end)
