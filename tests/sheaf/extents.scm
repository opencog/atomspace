
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

(define l2 (left-most-numa n2 two-arcs))
(define r2 (right-most-numa n2 two-arcs))

(define l3 (left-most-numa n3 two-arcs))
(define r3 (right-most-numa n3 two-arcs))

(define l4 (left-most-numa n4 two-arcs))
(define r4 (right-most-numa n4 two-arcs))

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

; Debug print
(print-wedglist three-arcs)

(define pl2 (left-most-numa n2 three-arcs))
(define pr2 (right-most-numa n2 three-arcs))

(define pl3 (left-most-numa n3 three-arcs))
(define pr3 (right-most-numa n3 three-arcs))

(define pl4 (left-most-numa n4 three-arcs))
(define pr4 (right-most-numa n4 three-arcs))

(test-equal "three-arc l2" pl2 lex)
(test-equal "three-arc r2" pr2 rex)

(test-equal "three-arc l3" pl3 lex)
(test-equal "three-arc r3" pr3 rex)

(test-equal "three-arc l4" pl4 lex)
(test-equal "three-arc r4" pr4 rex)

(test-end tloop)

; ---------------------------------------------------------------
(define reved "reversed edge test")
(test-begin reved)

(define reverso (list
	(mkw 3 (Concept "is") 2 (Concept "this") 3.2)
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") 1.2)
	(mkw 5 (Concept "test") 4 (Concept "a") 5.4)
	(mkw 3 (Concept "is") 4 (Concept "a") 3.4)
))

; Debug print
(print-wedglist reverso)

(define w1 (cons 1 (Concept "###LEFT-WALL###")))
(define w2 (cons 2 (Concept "This")))
(define w3 (cons 3 (Concept "is")))
(define w4 (cons 4 (Concept "a")))
(define w5 (cons 5 (Concept "test")))

(define wl1 (left-most-numa w1 reverso))
(define wr1 (right-most-numa w1 reverso))

(define wl2 (left-most-numa w2 reverso))
(define wr2 (right-most-numa w2 reverso))

(define wl3 (left-most-numa w3 reverso))
(define wr3 (right-most-numa w3 reverso))

(define wl4 (left-most-numa w4 reverso))
(define wr4 (right-most-numa w4 reverso))

(define wl5 (left-most-numa w5 reverso))
(define wr5 (right-most-numa w5 reverso))

; what we expect
(define lwex w1)
(define rwex w5)

(test-equal "reversed-edge l1" wl1 lwex)
(test-equal "reversed-edge r1" wr1 rwex)

(test-equal "reversed-edge l2" wl2 lwex)
(test-equal "reversed-edge r2" wr2 rwex)

(test-equal "reversed-edge l3" wl3 lwex)
(test-equal "reversed-edge r3" wr3 rwex)

(test-equal "reversed-edge l4" wl4 lwex)
(test-equal "reversed-edge r4" wr4 rwex)

(test-equal "reversed-edge l5" wl5 lwex)
(test-equal "reversed-edge r5" wr5 rwex)

(test-end reved)

; ---------------------------------------------------------------
(opencog-test-end)
