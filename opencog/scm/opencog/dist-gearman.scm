;
; OpenCog Gearman module
;
(define-module (opencog dist-gearman))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-dist-gearman "libdist-gearman") "opencog_dist_init")

(export start-work-handler dist-eval exit-all-workers)
