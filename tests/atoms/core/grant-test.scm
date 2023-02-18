;
; grant-test.scm -- test that GrantLink works.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "grant-test")
(test-begin tname)

(define grant (Grant (Concept "A") (Concept "B")))
(test-assert "is-key" (equal? (Concept "A") (cog-outgoing-atom grant 0)))
(test-assert "is-val" (equal? (Concept "B") (cog-outgoing-atom grant 1)))

(define gr2 (Grant (Concept "A") (Concept "C")))
(test-assert "is-key2" (equal? (Concept "A") (cog-outgoing-atom gr2 0)))
(test-assert "is-val2" (equal? (Concept "B") (cog-outgoing-atom gr2 1)))

(cog-push-atomspace)
(define gr3 (Grant (Concept "A") (Concept "D")))
(test-assert "is-key3" (equal? (Concept "A") (cog-outgoing-atom gr3 0)))
(test-assert "is-val3" (equal? (Concept "B") (cog-outgoing-atom gr3 1)))

(cog-push-atomspace)
(define gr4 (Grant (Concept "A") (Concept "E")))
(test-assert "is-key4" (equal? (Concept "A") (cog-outgoing-atom gr4 0)))
(test-assert "is-val4" (equal? (Concept "B") (cog-outgoing-atom gr4 1)))

(cog-pop-atomspace)
(define gr5 (Grant (Concept "A") (Concept "F")))
(test-assert "is-key5" (equal? (Concept "A") (cog-outgoing-atom gr5 0)))
(test-assert "is-val5" (equal? (Concept "B") (cog-outgoing-atom gr5 1)))

(cog-pop-atomspace)
(define gr6 (Grant (Concept "A") (Concept "F")))
(test-assert "is-key6" (equal? (Concept "A") (cog-outgoing-atom gr6 0)))
(test-assert "is-val6" (equal? (Concept "B") (cog-outgoing-atom gr6 1)))

(test-assert "is-key" (equal? (Concept "A") (cog-outgoing-atom grant 0)))
(test-assert "is-val" (equal? (Concept "B") (cog-outgoing-atom grant 1)))

(test-end tname)

(opencog-test-end)
