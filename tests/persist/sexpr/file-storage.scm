
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog persist-file))

(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "store_load_file")
(test-begin tname)

(test-assert "stuff" #t)

(test-end tname)
