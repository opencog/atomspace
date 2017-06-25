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
; The functions below provide convenient access to a "weighted
; vertex-ordered graph". A "vertex-ordered graph" is a graph where
; each vertex in the graph is labelled with an ordinal. This ordering
; can be imagined to give the vertexes a left-to-right ordering.
; The graph is weighted if each of teh edges is assigned a weight,
; (equivalently, a "cost").
;
; The functions below simply provide an API to access the ordering and
; the weights.
;
; Terminology:
; A "numa" is a numbered atom; it is an ordered vertex. Its an atom,
;    and an intgeger number indicating it's ordering.
;
; An "ovirt" is the same thing as a numa.
;
; A "wlink" is an edge, consisting of an ordered pair of numa's.
;     Note that ordereing of the vertexes in the edge give that
;     edge an implicit directionality. This need NOT correspond
;     to the ordinal numbering of the vertexes. That is, an edge
;     can point from right to left or from left to right!
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))

; ---------------------------------------------------------------------
; The MST parser returns a list of weighted edges, each edge consisting
; of a pair of ordered atoms.
; The functions below unpack each data strcture.
;
; Get the score of the link.
(define-public (mst-link-get-score lnk) (cdr lnk))

; Get the left numbered-atom (numa) in the link. The num is a scheme
; pair of the form (number . atom)
(define-public (mst-link-get-left-ovirt lnk)
	(car (car lnk)))

(define-public (mst-link-get-right-ovirt lnk)
	(cdr (car lnk)))

; Get the index number out of the numa.
(define-public (ovirt-get-index numa) (car numa))

; Get the atom from the numa.
(define-public (ovirt-get-atom numa) (cdr numa))

; Get the left atom in the scored link.
(define-public (mst-link-get-left-atom lnk)
	(ovirt-get-atom (mst-link-get-left-ovirt lnk)))

; Get the right word in the link. This returns the WordNode.
(define-public (mst-link-get-right-atom lnk)
	(ovirt-get-atom (mst-link-get-right-ovirt lnk)))

; ---------------------------------------------------------------------
