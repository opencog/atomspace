;
; config.scm
;
; General cog-server configuration, for scheme.
;--------------------------------------------------------------
;
; Saying `(use-modules (opencog blah))` at the cogserver prompt
; will blow up without this.
(add-to-load-path "/usr/local/share/opencog/scm")
;
; The scheme shell listen port.
(define shell-port 18001)
; A plain simple prompt
; (define shell-prompt "opencog-scheme> ")
; An ANSI-terminal colorized prompt!  This one is blue.
(define shell-prompt "[0;34mopencog-scheme[1;34m> [0m")
