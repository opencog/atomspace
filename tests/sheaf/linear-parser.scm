
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog sheaf))

(opencog-test-runner)

(define tbridge "bridge-parser-test")
(test-begin tbridge)

(define wordli (list
	"###LEFT-WALL###" "this" "is" "a" "kind" "of" "test" "it" "seems"))
(define atomli (map Concept wordli))
(define nali (atom-list->numa-list atomli))

; make a wedge...
(define (mkw NA WA NB WB SC) (cons (cons (cons NA WA) (cons NB WB)) SC))

; Make a graph with four islands
(define islands (list
   (mkw 1 (ConceptNode "###LEFT-WALL###") 2 (ConceptNode "this") 1.2)
   (mkw 3 (ConceptNode "is") 4 (ConceptNode "a") 3.4)
   (mkw 5 (ConceptNode "kind") 6 (ConceptNode "of") 5.6)
   (mkw 7 (ConceptNode "test") 8 (ConceptNode "it") 7.8)))

; Debug print
(print-wedglist islands)

; Bridge all the islands
(define connected (sort-wedgelist (graph-add-bridges islands)))

; This is what we expect to get back.
(define expected
   (mkw 1 (ConceptNode "###LEFT-WALL###") 2 (ConceptNode "this") 1.2)
   (mkw 2 (ConceptNode "this") 3 (ConceptNode "is") -inf.0)
   (mkw 3 (ConceptNode "is") 4 (ConceptNode "a") 3.4)
   (mkw 4 (ConceptNode "a") 5 (ConceptNode "kind") -inf.0)
   (mkw 5 (ConceptNode "kind") 6 (ConceptNode "of") 5.6)
   (mkw 6 (ConceptNode "of") 7 (ConceptNode "test") -inf.0)
   (mkw 7 (ConceptNode "test") 8 (ConceptNode "it") 7.8))

(test-equal "Island hopper" connected expected)

(test-end tbridge)
