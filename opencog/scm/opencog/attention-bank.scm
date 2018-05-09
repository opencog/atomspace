;
; OpenCog AttentionBank module
; Copyright (C) 2018 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog attention-bank))

(load-extension "libattentionbank" "opencog_attention_init")

; This avoids complaints, when the docs are set, below.
(export bcog-av)

(use-modules (opencog))

;; -----------------------------------------------------
;;

(set-procedure-property! bcog-av 'documentation
"
 bccog-av ATOM
    foo
")

; --------------------------------------------------------------------
