;
; proxy-node-test.scm -- Unit test for basic ProxyNode syntax.
;
(use-modules (opencog) (opencog persist))
(use-modules (opencog exec))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "proxy-node")
(test-begin tname)

; This should lead to failure.
(define foo (WriteThruProxy "foo"))
(ProxyParametersLink foo (Concept "bar"))

(define caught #f)
(catch 'C++-EXCEPTION (lambda () (cog-open foo))
	(lambda (key funcname msg)
		(format #t "Yes, we got the exception as expected\n")
		(set! caught #t)))

(test-assert "Failed to catch exception" caught)

; ----------------------------
; Assorted permutations -- just one target

(define wnull (WriteThruProxy "wnull"))
(ProxyParametersLink wnull (NullProxy "bar"))

(cog-open wnull)
(store-atom (Concept "boffo"))
(cog-close wnull)

(set! caught #f)
(catch 'C++-EXCEPTION (lambda () (store-atom (Concept "boffo")))
	(lambda (key funcname msg)
		(format #t "Yes, we got the exception as expected\n")
		(set! caught #t)))

(test-assert "Failed to catch exception" caught)

; ----------------------------
; Multiple targets

(define wmulti (WriteThruProxy "wmulti"))
(ProxyParametersLink wmulti
	(List
		(NullProxy "foo")
		(NullProxy "bar")
		(NullProxy "baz")))

(cog-open wmulti)
(store-atom (Concept "boffo"))
(cog-close wmulti)

(set! caught #f)
(catch 'C++-EXCEPTION (lambda () (store-atom (Concept "boffo")))
	(lambda (key funcname msg)
		(format #t "Yes, we got the exception as expected\n")
		(set! caught #t)))

(test-assert "Failed to catch exception" caught)


; ----------------------------
; Assorted permutations -- just one reader

(define rnull (ReadThruProxy "wnull"))
(ProxyParametersLink rnull (NullProxy "bar"))

(cog-open rnull)
(fetch-atom (Concept "b1"))
(fetch-atom (Concept "b2"))
(fetch-atom (Concept "b3"))
(fetch-atom (Concept "b4"))
(cog-close rnull)

(set! caught #f)
(catch 'C++-EXCEPTION (lambda () (fetch-atom (Concept "boffo")))
	(lambda (key funcname msg)
		(format #t "Yes, we got the exception as expected\n")
		(set! caught #t)))

(test-assert "Failed to catch exception" caught)

; ----------------------------
; Multiple readers

(define rmulti (ReadThruProxy "wmulti"))
(ProxyParametersLink rmulti
	(List
		(NullProxy "foo")
		(NullProxy "bar")
		(NullProxy "baz")))

(cog-open rmulti)
(fetch-atom (Concept "b1"))
(fetch-atom (Concept "b2"))
(fetch-atom (Concept "b3"))
(fetch-atom (Concept "b4"))
(fetch-atom (Concept "b5"))
(fetch-atom (Concept "b6"))
(cog-close rmulti)

(set! caught #f)
(catch 'C++-EXCEPTION (lambda () (fetch-atom (Concept "boffo")))
	(lambda (key funcname msg)
		(format #t "Yes, we got the exception as expected\n")
		(set! caught #t)))

(test-assert "Failed to catch exception" caught)



(test-end tname)

(opencog-test-end)
