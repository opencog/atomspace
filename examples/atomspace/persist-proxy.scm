;
; persist-proxy.scm -- Using proxy agents to automate storage.
;
; This demo illustrates how to use proxy agents to move Atoms around
; from one storage location to another. The primary demo is of a network
; server that is serving Atoms from off the disk. The idea is that when
; the server is started, it's AtomSpace is empty, and all of the Atoms
; are sitting on disk. When the first client attaches and asks for some
; Atom, the server has to go and grab it from the disk, first. The
; `ReadThruProxyNode` can do this: when it sees that request, it turns
; around and passes it forward to the disk StorageNode. Once fetched
; from disk, it can then be returned by network to the requesting client.
;
; Proxy agents currently include:
;
; * ReadThruProxy -- Passes on requests involving the reading of
;      Atoms and Values. This includes `fetch-atom`, `fetch-value`,  
;      `fetch-incoming-set` and `fetch-incoming-by-type`.
;
; * WriteThruProxy -- Passes on requests involving the storing of
;       Atoms and Values. This includes
