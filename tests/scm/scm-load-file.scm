(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog persist))
(use-modules (opencog persist-file))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "load_file")
(test-begin tname)

(format #t "The current directory is ~A\n" (getcwd))

; Fill atomspace from a file's content.
;
; Hack filename to go to the correct directory for the unit tests.
; This test runs in the build dir, and the file to load is in the
; source dir.
(load-file "../../../tests/scm/scm-load-file-test-data.scm")

(define names (map cog-name (cog-get-atoms "ConceptNode")))

(define (contains? el lst)
   (any (lambda (x) (equal? el x)) lst)
)

(define contain_all 
  (and (contains? "тестирование кода приводит к успеху" names)
       (contains? "Codeprüfung führt zum Erfolg" names)
       (contains? "'''''''''1;" names)
       (contains? "A" names)
       (contains? "B" names)
  ))

; would fail on slashes at the end of string: (ConceptNode "a\\")
(test-assert "load data from file" contain_all)

; Make sure correct number of atoms loaded:
(test-assert "correct number of atoms" (equal? 27 (count-all)))

(test-assert "Concept" (equal? 10 (cog-count-atoms 'Concept)))
(test-assert "Predicate" (equal? 2 (cog-count-atoms 'Predicate)))
(test-assert "List" (equal? 5 (cog-count-atoms 'List)))
(test-assert "Evaluation" (equal? 3 (cog-count-atoms 'Evaluation)))
(test-assert "Member" (equal? 4 (cog-count-atoms 'Member)))
(test-assert "Lexical" (equal? 3 (cog-count-atoms 'Lexical)))

(test-end tname)

(opencog-test-end)
