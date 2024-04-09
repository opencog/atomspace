(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "copy-atoms")
(test-begin tname)

(define ca (Concept "A"))

(test-assert "created atom" (not (nil? ca)))
(test-assert "its an atom" (cog-atom? ca))
(test-assert "concept exists" (equal? ca (cog-atom ca)))

(define spacex (cog-new-atomspace))
(test-assert "new atmspace is different"
	(not (equal? spacex (cog-atomspace))))

(define xca (cog-new-atom ca spacex))

(test-assert "created copy" (not (nil? xca)))
(test-assert "copy is an atom" (cog-atom? xca))
(test-assert "copy exists" (cog-equal? xca (cog-atom xca)))

(test-assert "original and copy content compare" (cog-equal? xca ca))
(test-assert "original and copy differ" (not (eq? xca ca)))
(test-assert "original and copy in different atmspaces"
	(not (eq? (cog-atomspace xca) (cog-atomspace ca))))

(test-assert "original is in correct atomspace"
	(equal? (cog-atomspace) (cog-atomspace ca)))

(test-assert "copy is in correct atomspace"
	(equal? spacex (cog-atomspace xca)))

(cog-set-tv! ca (SimpleTruthValue 0.4 0.4))
(cog-set-tv! xca (SimpleTruthValue 0.7 0.7))

(test-assert "original and copy hold different things"
	(not (equal? (cog-tv ca) (cog-tv xca))))

(test-end tname)

(opencog-test-end)
