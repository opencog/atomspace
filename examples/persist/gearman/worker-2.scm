(use-modules (opencog dist-gearman))
(use-modules (srfi srfi-1))

(load "task.scm")

(start-work-handler "localhost" "worker-2")
