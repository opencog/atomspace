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

(define-public (graph-add-linear GRAPH NUMA-LIST)
"
  Linear, Sequential Graph (LSG) parser.

  Given an existing GRAPH, create a connected graph by attaching any
  unconnected Atoms in the NUMA-LIST with new edges. Attachments are
  made in sequential order, thus preserving the order of the NUMA-LIST
  and preserving the planarity (if any) of the original graph.

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

  If the graph has a bridge over a sequence of unconnected nodes,
  then a loop will be created, as those unconnected nodes will be
  attached to both the left and to the right.

  If the graph has multiple components (islands), but no disconnected
  nodes, then the returned result will remain disconnected. Use
  `graph-add-bridges` to connect together islands. But if there is
  a disconnected node between two islands, then that node will be
  attached to both islands, thus connecting them.  Islands remain
  unconnected only if nothing is between them.
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

	; This attaches all of the disconnected nodes to some existing
	; graph component. However, the graph may still have multiple
	; disjoint components. To bad; we return that.
	(*join-em-up GRAPH '() '() alldem graver discon)
)

; ---------------------------------------------------------------------

(define-public (graph-add-bridges GRAPH)
"
  Sequential Island Bridger (SIB) parser.

  Given an existing GRAPH which may contain disconnected components
  or 'islands', this will add edges that connect neighboring islands.

  The GRAPH should be an existing, non-empty list of 'wedges'.

  This returns a new list of wedges, such that the resulting graph
  is simply connected.

  XXX FIXME WARNING DANGER: As written, this runs in exponential time
  as the size of the graph (the wedges), and thus can explode in
  runtime, going from a fraction of a second for one graph, and many
  minutes (or hours) for a graph that is 20% bigger!  This makes this
  function unusable. You've been warned!
"
	(define (make-wedge VA VB) (cons (cons VA VB) -inf.0))

	; Place vertexes in sorted order.
	(define sorted-numas (sort-numalist (numas-in-wedge-list GRAPH)))

	; Find the right-most vertex connected to the left-most one.
	(define right-end
		(if (null? sorted-numas) (cons -inf.0 #f)
			(right-most-numa (car sorted-numas) GRAPH)))

	; Its index.
	(define right-idx (numa-get-index right-end))

	; Drop all vertexes to the left.
	(define remainder (drop-while
		(lambda (numa) (<= (numa-get-index numa) right-idx))
		sorted-numas))

	; If we dropped all of them we are done.
	; Else, the first one is the start of a new island.
	(if (null? remainder) GRAPH
		(let ((gap (left-most-numa (car remainder) GRAPH)))
			(graph-add-bridges (cons (make-wedge right-end gap) GRAPH))))
)

; ---------------------------------------------------------------------
