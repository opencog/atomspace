;
; cover-basic-delete-test.scm
; Verify that deletion works in a "common sense" fashion when
; atomspaces are layers.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

; -------------------------------------------------------------------
; Common setup, used by all tests.

(define lower-space (cog-atomspace))
(define upper-space (cog-new-atomspace lower-space))

; ===================================================================
; Test basic extraction.

(define basic-lo-delete "test basic extract from below")
(test-begin basic-lo-delete)
(begin

	; Setup ----
	(cog-set-atomspace! lower-space)
	(define a (Concept "a"))
	(define b (Concept "b"))

	(cog-set-atomspace! upper-space)
	(define li (Link a b))

	; Test ---
	(cog-set-atomspace! lower-space)

	(test-equal "lower-size" 2 (length (cog-get-atoms 'Atom #t)))
	(test-assert "extract-fail" (not (cog-extract! a)))
	(test-equal "ex-lower-size" 2 (length (cog-get-atoms 'Atom #t)))

	(test-assert "extract-works" (cog-extract-recursive! a))
	(test-equal "post-ex-lower-size" 1 (length (cog-get-atoms 'Atom #t)))

	(test-equal "empty-income" 0 (length (cog-incoming-set b)))

	; Because the upper space was not protected, we expect the recursive
	; delete to have blown away the link. Protection requires configing
	; the COW flag correctly.
	(cog-set-atomspace! upper-space)
	(test-equal "upper-blown" 1 (length (cog-get-atoms 'Atom #t)))
	(test-equal "upper-b-only" (list b) (cog-get-atoms 'Atom #t))
)
(test-end basic-lo-delete)

; ===================================================================
; Same as above, except this time, extract from the upper space.

(define basic-hi-delete "test basic extract from above")
(test-begin basic-hi-delete)
(begin

	; Setup ----
	(cog-set-atomspace! lower-space)
	(define a (Concept "a"))
	(define b (Concept "b"))

	; Prevent any further modification
	(cog-atomspace-ro! lower-space)

	(cog-set-atomspace! upper-space)
	(define li (Link a b))

	; upper space should be COW
	(cog-atomspace-cow! #t upper-space)

	; Test ---
	(cog-set-atomspace! lower-space)

	(test-equal "lower-size" 2 (length (cog-get-atoms 'Atom #t)))
	(cog-set-atomspace! upper-space)
	(test-equal "upper-size" 3 (length (cog-get-atoms 'Atom #t)))

	(test-assert "extract-fail" (not (cog-extract! a)))
	(test-equal "ex-upper-size" 3 (length (cog-get-atoms 'Atom #t)))

	(test-assert "extract-works" (cog-extract-recursive! a))
	(test-equal "post-ex-upper-size" 1 (length (cog-get-atoms 'Atom #t)))
	(test-equal "empty-income" 0 (length (cog-incoming-set b)))

	; The lower lower space was protected. Bot a and b should be there.
	(cog-set-atomspace! lower-space)
	(test-equal "lower-ok" 2 (length (cog-get-atoms 'Atom #t)))
)
(test-end basic-hi-delete)

; ===================================================================
; Test clean extraction under covers.

(define basic-clean-delete "test clean extraction below")
(test-begin basic-clean-delete)
(begin

	; Setup ----
	(cog-set-atomspace! lower-space)
	(cog-atomspace-rw! lower-space)
	(cog-atomspace-cow! #t lower-space)
	(define a (Concept "a"))
	(define b (Concept "b"))

	(cog-set-atomspace! upper-space)
	(cog-atomspace-cow! #t upper-space)
	(define li (Link a b))

	; Test ---
	(cog-set-atomspace! lower-space)

	(test-equal "lower-size" 2 (length (cog-get-atoms 'Atom #t)))
	(test-assert "extract-works" (cog-extract! a))
	(test-equal "ex-lower-size" 1 (length (cog-get-atoms 'Atom #t)))
	(test-equal "lower-b-only" (list b) (cog-get-atoms 'Atom #t))
	(test-equal "empty-income" 0 (length (cog-incoming-set b)))

	; The upper space was COW, we expect the link to still be there.
	(cog-set-atomspace! upper-space)
	(test-equal "upper-OK" 3 (length (cog-get-atoms 'Atom #t)))
	(test-equal "upper-OK-list" 1 (length (cog-get-all-roots)))
	(test-equal "upper-list" (list li) (cog-get-all-roots))
)
(test-end basic-clean-delete)

; ===================================================================
(opencog-test-end)
