;
; repl-shell.scm
;
; Start the "high-level" scheme shell.
;
; Copyright (C) 2013 Linas Vepstas <linasvepstas@gmail.com>
;
; --------------------------------------------------------------------

(use-modules (system repl server))
(use-modules (system repl common))

; localhost, port number 18001
(spawn-server (make-tcp-server-socket  #:port 18001))

; A plain simple prompt
; (define shell-prompt "opencog-scheme> ")

; An ANSI-terminal colorized prompt!  This one is blue.
(define shell-prompt "[0;34mopencog-scheme[1;34m> [0m")

(repl-default-prompt-set! shell-prompt)
; --------------------------------------------------------------------
