;
; OpenCog Logger module
;

(define-module (opencog logger))

(load-extension "libsmob" "opencog_logger_init")

;
;;; Commentary:
;
; Helper to use ice-9 format in logger functions
;
;;; Code:
; Copyright (c) 2015, OpenCog Foundation
;

(use-modules (ice-9 format))

(define (cog-logger-error msg . args)
  (cog-logger-error-str (apply format #f msg args)))

(define (cog-logger-warn msg . args)
  (cog-logger-warn-str (apply format #f msg args)))

(define (cog-logger-info msg . args)
  (cog-logger-info-str (apply format #f msg args)))

(define (cog-logger-debug msg . args)
  (cog-logger-debug-str (apply format #f msg args)))

(define (cog-logger-fine msg . args)
  (cog-logger-fine-str (apply format #f msg args)))

(export cog-logger-error
        cog-logger-warn
        cog-logger-info
        cog-logger-debug
        cog-logger-fine)
