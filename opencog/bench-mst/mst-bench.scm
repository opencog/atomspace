;
; mst-bench.scm
;
; A sloppy, cheesey, quick-n-dirty tool to measure MST performance.
; Linas Vepstas August 2017
;

(use-modules (opencog) (opencog sheaf))

; Step 1: create a bunch of random pair data.

; Create MPAIRS different random pairs, connecting NVERTS different
; vertexes.
(define (make-words MPAIRS NVERTS)
	(define (mkvert)
		(Concept (format #f "vertex ~D" (random NVERTS))))

	(define (mkpair)
		(Evaluation (Predicate "benchy") (List (mkvert) (mkvert))))

)
