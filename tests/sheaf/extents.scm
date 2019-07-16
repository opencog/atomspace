
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog sheaf))

(opencog-test-runner)

; ---------------------------------------------------------------
; This tests the extent-finding code. This is code that traverses
; the graph to find the left-most and right-most extends of the graph.
; It needs to find the right answer and also not hang in case the graph
; contains loops.

; Common setup used by all tests.
(define wordli (list "###LEFT-WALL###" "Part" "Three" "."))
(define atomli (map Concept wordli))
(define nali (atom-list->numa-list atomli))

; Make a wedge...
(define (mkw NA WA NB WB SC) (cons (cons (cons NA WA) (cons NB WB)) SC))

; ---------------------------------------------------------------
(define tgra "simple graph test")
(test-begin tgra)

(define two-arcs (list
	(mkw 2 (Concept "Part") 3 (Concept "Three") 2.3)
	(mkw 2 (Concept "Part") 4 (Concept ".") 2.4)
))

(define n2 (cons 2 (Concept "Part")))
(define n3 (cons 3 (Concept "Three")))
(define n4 (cons 4 (Concept ".")))

; Debug print
(print-wedglist two-arcs)

(define l2 (left-most-numa n2 two-arcs)
(define r2 (right-most-numa n2 two-arcs)

(define l3 (left-most-numa n3 two-arcs)
(define r3 (right-most-numa n3 two-arcs)

(define l4 (left-most-numa n4 two-arcs)
(define r4 (right-most-numa n4 two-arcs)

; what we expect
(define lex n2)
(define rex n4)

(test-equal "two-arc l2" l2 lex)
(test-equal "two-arc r2" r2 rex)

(test-equal "two-arc l3" l3 lex)
(test-equal "two-arc r3" r3 rex)

(test-equal "two-arc l4" l4 lex)
(test-equal "two-arc r4" r4 rex)

(test-end tgra)

; ---------------------------------------------------------------
(define tloop "loopy graph test")
(test-begin tloop)

(define three-arcs (list
	(mkw 2 (Concept "Part") 3 (Concept "Three") 2.3)
	(mkw 2 (Concept "Part") 4 (Concept ".") 2.4)
	(mkw 3 (Concept "Three") 2 (Concept "Part") 3.2)
))

(define n2 (cons 2 (Concept "Part")))
(define n3 (cons 3 (Concept "Three")))
(define n4 (cons 4 (Concept ".")))

; Debug print
(print-wedglist three-arcs)

(define l2 (left-most-numa n2 three-arcs)
(define r2 (right-most-numa n2 three-arcs)

(define l3 (left-most-numa n3 three-arcs)
(define r3 (right-most-numa n3 three-arcs)

(define l4 (left-most-numa n4 three-arcs)
(define r4 (right-most-numa n4 three-arcs)

; what we expect
(define lex n2)
(define rex n4)

(test-equal "three-arc l2" l2 lex)
(test-equal "three-arc r2" r2 rex)

(test-equal "three-arc l3" l3 lex)
(test-equal "three-arc r3" r3 rex)

(test-equal "three-arc l4" l4 lex)
(test-equal "three-arc r4" r4 rex)

(test-end tloop)

; ---------------------------------------------------------------
