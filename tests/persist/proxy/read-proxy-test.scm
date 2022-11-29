;
; read-proxy-test.scm -- Unit test for ReadThru and WriteThru proxies.
;
(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-sql))
(use-modules (opencog exec))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "read-proxy-test")
(test-begin tname)

; Could be any StorageNode, e.g. RocksStorageNode, etc.
; but, for convenience, we just use Postgres for now.
(define sto (PostgresStorageNode
	"postgres:///opencog_test?user=opencog_tester&password=cheese"))

(define wsto (WriteThruProxy "writer"))
(ProxyParametersLink wsto sto)

(cog-open wsto)
(store-atom (Concept "b1" (stv 0.1 0.1)))
(store-atom (Concept "b2" (stv 0.2 0.2)))
(store-atom (Concept "b3" (stv 0.3 0.3)))
(store-atom (Concept "b4" (stv 0.4 0.4)))
(cog-close wsto)

; ----------------------------
; Now, read it back.
; But first, clear the atomspace
(cog-atomspace-clear)

(set! sto (PostgresStorageNode
	"postgres:///opencog_test?user=opencog_tester&password=cheese"))

(define rsto (ReadThruProxy "reader"))
(ProxyParametersLink rsto sto)

(cog-open rsto)
(fetch-atom (Concept "b1"))
(fetch-atom (Concept "b2"))
(fetch-atom (Concept "b3"))
(fetch-atom (Concept "b4"))
(cog-close rsto)

(test-approximate "b1" 0.1 (cog-mean (Concept "b1")) 1.0e-8)
(test-approximate "b2" 0.2 (cog-mean (Concept "b2")) 1.0e-8)
(test-approximate "b3" 0.3 (cog-mean (Concept "b3")) 1.0e-8)
(test-approximate "b4" 0.4 (cog-mean (Concept "b4")) 1.0e-8)

(test-end tname)

(opencog-test-end)
