;
; vo-graph.scm
;
; Vertex-ordered graph.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The functions below provide convenient access to a "vertex-ordered
; graph"
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))

; ---------------------------------------------------------------------
; The MST parser returns a list of word-pair links, tagged with the
; mutual information between that word-pair. The functions below
; unpack each data strcture.
;
; Get the score of the link.
(define-public (mst-link-get-score lnk) (cdr lnk))

; Get the left numbered-atom (numa) in the link. The num is a scheme
; pair of the form (number . atom)
(define-public (mst-link-get-left-numa lnk)
	(car (car lnk)))

(define-public (mst-link-get-right-numa lnk)
	(cdr (car lnk)))

; Get the index number out of the numa.
(define-public (mst-numa-get-index numa) (car numa))

; Get the atom from the numa.
(define-public (mst-numa-get-atom numa) (cdr numa))

; Get the left atom in the scored link.
(define-public (mst-link-get-left-atom lnk)
	(mst-numa-get-atom (mst-link-get-left-numa lnk)))

; Get the right word in the link. This returns the WordNode.
(define-public (mst-link-get-right-atom lnk)
	(mst-numa-get-atom (mst-link-get-right-numa lnk)))

; Return the word-pair of the mst-link, as a listLink of WorNodes.
(define (mst-link-get-wordpair lnk)
	(ListLink (mst-link-get-left-atom lnk) (mst-link-get-right-atom lnk))
)

; ---------------------------------------------------------------------
