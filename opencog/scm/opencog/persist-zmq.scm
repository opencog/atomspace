;
; OpenCog ZeroMQ Persistence module
;

(define-module (opencog persist-zmq))

(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-persist-zmq "libpersist-zmq") "opencog_persist_zmq_init")
