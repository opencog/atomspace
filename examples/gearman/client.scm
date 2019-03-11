(use-modules (opencog dist-gearman))
(use-modules (ice-9 threads))

(define (send-work x) (dist-eval "(do-work 120 120)" "client"))
(define result (par-map send-work (iota 10)))

;; (exit-all-workers)
