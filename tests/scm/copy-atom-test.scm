(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "copy-atoms-test")
(test-begin tname)

(define ca (Concept "A"))

(test-assert "created atom" (not (nil? ca)))
(test-assert "its an atom" (cog-atom? ca))
(test-assert "concept exists" (equal? ca (cog-atom ca)))

(define spacex (cog-new-atomspace))
(test-assert "new atomspace is different"
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

; -------------------------------------------------------
; Now test copying of orphaned atoms (Atoms not in any Atomspace)

(cog-push-atomspace)
(define lv (LinkValue (Concept "foo") (Concept "bar")))
(cog-pop-atomspace)

(define ilst (cog-value->list lv))

(for-each
	(lambda (orphan)
		; cog-atom? must return #t because they are Atoms
		(test-assert "expect actual atoms" (cog-atom? orphan)))
	ilst)

; Print the ilst. This will clobber the handles, because the scheme
; API does not allow scheme smobs with orphan Atoms in them.
(format #t "Should be invalid: ~A\n" ilst)

(for-each
	(lambda (orphan)
		; cog-atom? must return #f because now, ilst is clobbered.
		(test-assert "expect invalid handles" (not (cog-atom? orphan))))
	ilst)

; The stuff in the LinkValue should be OK, still.
(for-each
	(lambda (orphan)
		; cog-atom? must return #t because they are Atoms
		(test-assert "expect actual atoms" (cog-atom? orphan)))
	(cog-value->list lv))

(for-each
	(lambda (orphan)
		; cog-atom must return #f because they're not in this atomspace.
		(test-assert "expect not in this atomspace" (not (cog-atom orphan))))
	(cog-value->list lv))

(define num-atoms (count-all))
(define alst (map cog-new-atom (cog-value->list lv)))
(define num-new (count-all))
(test-assert "increase by two" (equal? 2 (- num-new num-atoms)))

(for-each
	(lambda (orphan)
		; They are still atoms
		(test-assert "expect valid atom" (cog-atom? orphan)))
	alst)

(for-each
	(lambda (orphan)
		; The must now be in this atomspace.
		(test-assert "expect in this atomspace" (cog-atom orphan)))
	alst)

; Do it again.
(define blst (map cog-new-atom (cog-value->list lv)))
(define num-again (count-all))
(test-assert "no change" (equal? 0 (- num-again num-new)))

(test-assert "Same atom 1" (equal? (first alst) (first blst)))
(test-assert "Same atom 2" (equal? (second alst) (second blst)))

(test-end tname)

(opencog-test-end)
