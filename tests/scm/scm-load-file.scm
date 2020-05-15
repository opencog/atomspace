(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog persist))
(use-modules (opencog test-runner))

(define (contains? el lst)
    (any (lambda (x) (equal? el x)) lst)
)

(opencog-test-runner)

(define tname "load_file")
(test-begin tname)

; fill atomspace from a file's content
(load-file "scm-load-file-test-data.scm")

(define names (map cog-name (cog-get-atoms "ConceptNode")))

(define contain_all 
  (and (contains? "тестирование кода приводит к успеху" names)
       (contains? "Codeprüfung führt zum Erfolg" names)
       (contains? "'''''''''1;" names)))

; would fail on slashes at the end of string: (ConceptNode "a\\")
(test-assert "load data from file"
  contain_all
)

(test-end tname)
