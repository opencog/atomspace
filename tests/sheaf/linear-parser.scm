
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog sheaf))

(opencog-test-runner)

; ---------------------------------------------------------------
; The tests here are some simple tests of the two simplest parsers
; -- the ones that fill in any gaps that might arise from using
; the more complex parsers.  There are two:
; * The bridger, which connects disconnected islands
; * The linear parser, which builds linear sequences.

; Common setup used by all tests.
(define wordli (list
	"###LEFT-WALL###" "this" "is" "a" "kind" "of" "test" "it" "seems"))
(define atomli (map Concept wordli))
(define nali (atom-list->numa-list atomli))

; Make a wedge...
(define (mkw NA WA NB WB SC) (cons (cons (cons NA WA) (cons NB WB)) SC))

; ---------------------------------------------------------------
(define tbridge "bridge-parser-test")
(test-begin tbridge)

; Make a graph with four islands
(define islands (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") 1.2)
	(mkw 3 (Concept "is")	4 (Concept "a") 3.4)
	(mkw 5 (Concept "kind") 6 (Concept "of") 5.6)
	(mkw 7 (Concept "test") 8 (Concept "it") 7.8)))

; Debug print
(print-wedglist islands)

; Bridge all the islands
(define connected (sort-wedgelist (graph-add-bridges islands)))

; This is what we expect to get back.
(define expected (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") 1.2)
	(mkw 2 (Concept "this") 3 (Concept "is")   -inf.0)
	(mkw 3 (Concept "is")	4 (Concept "a")     3.4)
	(mkw 4 (Concept "a")    5 (Concept "kind") -inf.0)
	(mkw 5 (Concept "kind") 6 (Concept "of")    5.6)
	(mkw 6 (Concept "of")	7 (Concept "test") -inf.0)
	(mkw 7 (Concept "test") 8 (Concept "it")    7.8)))

(test-equal "Island hopper" connected expected)

; -----------------
; Pathological test... must not fail.
(define empty (graph-add-bridges '()))
(test-equal "Empty Islands" empty '())

(test-end tbridge)

; ---------------------------------------------------------------
(define tlinear "linear-parser-test")
(test-begin tlinear)

; One sole edge
(define midli (list
	(mkw 4 (Concept "a") 5 (Concept "kind") 4.5)))

(define midlin (sort-wedgelist (graph-add-linear midli nali)))

; This is what we expect
(define midexp (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") -inf.0)
	(mkw 2 (Concept "this") 3 (Concept "is")    -inf.0)
	(mkw 3 (Concept "is")	4 (Concept "a")     -inf.0)
	(mkw 4 (Concept "a")    5 (Concept "kind")   4.5)
	(mkw 5 (Concept "kind") 6 (Concept "of")    -inf.0)
	(mkw 6 (Concept "of")	7 (Concept "test")  -inf.0)
	(mkw 7 (Concept "test") 8 (Concept "it")    -inf.0)
	(mkw 8 (Concept "it")	9 (Concept "seems") -inf.0)))


(test-equal "Linear Midland" midlin midexp)

; ----------------
; Two island edges
(define islli (list
	(mkw 3 (Concept "is")   4 (Concept "a")  3.4)
	(mkw 5 (Concept "kind") 6 (Concept "of") 5.6)))

(define isllin (sort-wedgelist (graph-add-linear islli nali)))

; This is what we expect
(define islexp (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") -inf.0)
	(mkw 2 (Concept "this") 3 (Concept "is")    -inf.0)
	(mkw 3 (Concept "is")	4 (Concept "a")      3.4)
	(mkw 5 (Concept "kind") 6 (Concept "of")     5.6)
	(mkw 6 (Concept "of")	7 (Concept "test")  -inf.0)
	(mkw 7 (Concept "test") 8 (Concept "it")    -inf.0)
	(mkw 8 (Concept "it")	9 (Concept "seems") -inf.0)))

(test-equal "Linear Island" isllin islexp)

; ----------------
; Two distant island edges
(define disli (list
	(mkw 3 (Concept "is")   4 (Concept "a")    3.4)
	(mkw 6 (Concept "of")   7 (Concept "test") 6.7)))

(define dislin (sort-wedgelist (graph-add-linear disli nali)))

; This is what we expect
(define disexp (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") -inf.0)
	(mkw 2 (Concept "this") 3 (Concept "is")    -inf.0)
	(mkw 3 (Concept "is")	4 (Concept "a")      3.4)
	(mkw 4 (Concept "a")    5 (Concept "kind")  -inf.0)
	(mkw 5 (Concept "kind") 6 (Concept "of")    -inf.0)
	(mkw 6 (Concept "of")	7 (Concept "test")   6.7)
	(mkw 7 (Concept "test") 8 (Concept "it")    -inf.0)
	(mkw 8 (Concept "it")	9 (Concept "seems") -inf.0)))

(test-equal "Linear Distant Island" dislin disexp)

; ----------------
; Two bridged islands
(define brili (list
	(mkw 3 (Concept "is")   4 (Concept "a")    3.4)
	(mkw 3 (Concept "is")   7 (Concept "test") 3.7)
	(mkw 6 (Concept "of")   7 (Concept "test") 6.7)))

(define brilin (sort-wedgelist (graph-add-linear brili nali)))

; This is what we expect
(define briexp (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") -inf.0)
	(mkw 2 (Concept "this") 3 (Concept "is")    -inf.0)
	(mkw 3 (Concept "is")	4 (Concept "a")      3.4)
	(mkw 3 (Concept "is")   7 (Concept "test")   3.7)
	(mkw 4 (Concept "a")    5 (Concept "kind")  -inf.0)
	(mkw 5 (Concept "kind") 6 (Concept "of")    -inf.0)
	(mkw 6 (Concept "of")	7 (Concept "test")   6.7)
	(mkw 7 (Concept "test") 8 (Concept "it")    -inf.0)
	(mkw 8 (Concept "it")	9 (Concept "seems") -inf.0)))

(test-equal "Linear Bridged Island" brilin briexp)

; ----------------
; Empty Set

(define emplin (sort-wedgelist (graph-add-linear '() nali)))

; This is what we expect
(define empexp (list
	(mkw 1 (Concept "###LEFT-WALL###") 2 (Concept "this") -inf.0)
	(mkw 2 (Concept "this") 3 (Concept "is")    -inf.0)
	(mkw 3 (Concept "is")   7 (Concept "test")  -inf.0)
	(mkw 4 (Concept "a")    5 (Concept "kind")  -inf.0)
	(mkw 5 (Concept "kind") 6 (Concept "of")    -inf.0)
	(mkw 6 (Concept "of")	7 (Concept "test")  -inf.0)
	(mkw 7 (Concept "test") 8 (Concept "it")    -inf.0)
	(mkw 8 (Concept "it")	9 (Concept "seems") -inf.0)))

(test-equal "Linear Empty Set" emplin empexp)

(test-end tlinear)

; ---------------------------------------------------------------
(opencog-test-end)
