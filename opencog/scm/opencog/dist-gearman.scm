;
; OpenCog Pattern matcher module
;

(define-module (opencog dist-gearman))

(load-extension "libdist-gearman" "opencog_dist_init")

(export start-work-handler dist-eval exit-all-workers)
