;
; linear-parser.scm
;
; Sequential Graph parser.
;
; Copyright (c) 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The functions below accept a sequence of atoms, and an initial graph
; (possibly consisting of multiple, disconnected components), and attach
; any atoms not already in the graph to it, so that all atoms and all
; graph components are connected, in a linear, sequential order.
;
; The input is a sequence of atoms, and an initial graph. Unlike the
; graph algos, this one does not require a scoring function. In the
; prototypical usage, the initial graph was created to be maximal in
; some way, (e.g. using MST or MPG), but some edges were not scorable,
; leaving behind disconnected atoms or components. The functions here
; add edges, creating a single connected component, even if the added
; edges are not scorable. Rather than adding random edges, they are
; added in the original sequential order, thus preserving the original
; sequential nature of the input.
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))
(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define-public (graph-add-linear GRAPH NUMA-LIST ATTACH-LEFT ATTACH-RIGHT)
"
  Linear, Sequential Graph (LSG) parser.

  Given an existing GRAPH, create a connected graph by attaching any
  unconnected Atoms in the NUMA-LIST with new edges. Attachments are
  made in sequential order, thus preserving the order of the NUMA-LIST
  and preserving the planarity (if any) of the original graph.

  Depending on the structure of teh graph, there may be ambiguity as
  to whether attachments should be made to the left, or to the right.
  This can be controlled by setting ATTACH-LEFT and ATTACH-RIGHT to
  either #t or #f.  Setting both to #t might result in a final graph
  with loops. Setting both to #f is invalid.

  The GRAPH should be an existing (possibly empty) list of 'wedges'
  connecting Atom pairs. Each 'wedge' is a weighted pair of numbered
  atoms, having the scheme form of `((NL . AL) (NR . AR) . W)` where
  AL and AR are the left and right Atoms of the edge; NL and NR are
  ordinal numbers (integers), such that NL is less than NR, and W is
  a floating-point weight. The dot represents a scheme pair, built
  with `cons`.

  The NUMA-LIST should be a scheme-list of ordinally-numbered atoms.
  This should be a list of scheme pairs `(Num . Atom)` where `Num` is
  is an ordinal number, and `Atom` is some Atom.

  This returns a new graph, in the form of a wedge-list. The added
  edges will have a weight of minus-infinity.
"
	; Terminology:
	; A "numa" is a numbered atom, viz a scheme-pair (number . atom)
	; A "wedge" is a weighted edge, having the form
	;    ((left-numa . right-num) . weight).

	(define (make-wedge VA VB) (cons (cons VA VB) -inf.0))

	; Tail-recursive joiner-upper
	; Its complicated to explain.
	; Arguments:
	; result -- the final result.
	; to-at -- a disconnected vertex, to the left, that needs attaching
	; prev -- a vertex to the left that is part of the graph
	; verli -- vertex list to iterate over.
	; grali -- list of vertexes in the graph
	; disli -- list of vertexes not in the graph.
	; It is assumed the last three are in left-right sorted order.
	(define (*join-em-up result to-at prev verli grali disli)
		(cond
			; If we've got no more disconencted nodes, we are done.
			((or (null? disli) (null? verli)) result)

			; If there are no more graph nodes, then all that
			; remains are disconnected nodes. Attach them.
			((null? grali)
				(*join-em-up
					(if (null? prev) result
						(cons (make-wedge prev (car verli)) result))
					'() (car verli) (cdr verli) grali (cdr disli)))

			; Well, there's more to do, obviously...
			(else
				(let* ((vxit (car verli))
						(grit (car grali))
						(dsit (car disli))
						; If there's an unattached node to the left, attach it.
						(bigg (if (null? to-at) result
								(cons (make-wedge to-at vxit) result)))
					)
					(cond
						; If this is a graph node, just ignore it, and recurse.
						((equal? vxit grit)
							(*join-em-up bigg '() vxit (cdr verli) (cdr grali) disli))

						; If this is a disconnected node, and the previous
						; node was a graph node, then attach to the previous
						; node.
						((equal? vxit dsit)
							(*join-em-up
								(if (null? prev) bigg
									(cons (make-wedge prev vxit) bigg))
								vxit '() (cdr verli) grali (cdr disli)))

						; Either its a graph node, or its disconnected.
						; Nothing else is possible...
						(else (throw 'invalid-vertex 'graph-add-linear
							(format #f "Unexpected vertex ~A" vxit))))
	))))

	; An ordered list of numa's in the graph.
	(define graver (sort-numalist (numas-in-wedge-list GRAPH)))

	; An ordered list of num'a NOT in the graph.
	(define discon (sort-numalist (lset-difference equal? NUMA-LIST graver)))

	; All of them, sorted.
	(define alldem (sort-numalist NUMA-LIST))

	(*join-em-up GRAPH '() '() alldem graver discon)
)

; ---------------------------------------------------------------------
