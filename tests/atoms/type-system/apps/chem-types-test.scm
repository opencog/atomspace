#! /usr/bin/env -S guile -s
!#
;
; chem-types-test.scm
;
; Unit test for custom atom types with custom short names in Scheme.
; Based on examples/type-system/apps/hello-chem.scm
;
(use-modules (opencog))
(use-modules (opencog test-demo-types))
(use-modules (opencog test-runner))
(use-modules (srfi srfi-1))  ; for `any` function

(opencog-test-runner)

(define tname "chem-types-test")
(test-begin tname)

; Test 1: Create molecules using short-name constructors
(test-assert "Create water molecule using short names"
	(not (nil?
		(Molecule
			(SB
				(O "big man oxygen")
				(H "one proton"))
			(SB
				(O "big man oxygen")
				(H "another proton"))))))

; Test 2: Verify types are correct
; Custom-named types register with their short name as TYPE_NAME
(test-assert "Hydrogen atom has correct type"
	(equal? (cog-type (H "test-h")) 'H))

(test-assert "Oxygen atom has correct type"
	(equal? (cog-type (O "test-o")) 'O))

(test-assert "Carbon atom has correct type"
	(equal? (cog-type (C "test-c")) 'C))

; Test 3: Test bond types
(test-assert "Single bond has correct type"
	(equal? (cog-type (SB (C "c1") (H "h1"))) 'SB))

(test-assert "Double bond has correct type"
	(equal? (cog-type (DB (C "c1") (C "c2"))) 'DB))

(test-assert "Triple bond has correct type"
	(equal? (cog-type (TB (C "c1") (N "n1"))) 'TB))

(test-assert "Aromatic bond has correct type"
	(equal? (cog-type (AB (C "c1") (C "c2"))) 'AB))

; Test 4: Test molecule type
(test-assert "Molecule has correct type"
	(equal? (cog-type (Molecule (SB (C "c") (H "h")))) 'Molecule))

; Test 5: Test Carbon14 execution
(define radioactive (Carbon14Node "wood"))
(define decay-products (cog-execute! radioactive))

(test-assert "Carbon14 execution returns products"
	(not (nil? decay-products)))

; Test 6: Verify decay products contain Nitrogen14
(test-assert "Decay products contain Nitrogen14Node"
	(any (lambda (p)
		(and (cog-atom? p)
		     (equal? (cog-type p) 'Nitrogen14Node)))
		(cog-value->list decay-products)))

(test-end tname)

(opencog-test-end)
