;
; mpg-parser.scm
;
; Maximum Planar Graph parser.
;
; Copyright (c) 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The functions below accept a sequence of atoms, and create a
; planar graph (MPG), such that the edges of the graph maximize a
; scoring function.
;
; Input is a sequence of atoms, together with a scoring function for
; ordered pairs of atoms. In a prototypical usage, the scoring function
; will return the mutual information between a pair of atoms, and
; so the MPG graph is a planar graph (i.e. with loops) that is maximally
; connected, in such a way that the mutual information between pairs of
; atoms is maximized.
;
; The function can start with any existing graph, and add edges, until
; the desired number of edges have been added, or it's impossible to add
; more edges. In the prototypical usage, the starting graph is the
; maximum spanning tree (MST) graph. Thus, the MST graph uses up the
; most favored edges, and the MPG algo adds in the less-favored edges,
; preserving planarity.
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))

; ---------------------------------------------------------------------

(define-public (graph-add-mpg GRAPH NUMA-LIST SCORE-FN NUM-EDGES)
"
  Projective, Undirected Maximum Planar Graph (MPG) parser.

  Given an existing GRAPH, add up to NUM-EDGES additional edges, such
  that each added edge has the highest possible score, and no added
  edge intersects any existing edge.  The non-intersection constraint
  keeps the graph planar or projective. If NUM-EDGES is set to -1,
  then as many edges as possible are added, resulting in the maximal
  planar graph.

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

  The SCORE-FN should be a function that, when give a left-right ordered
  pair of atoms, and the distance between them, returns a numeric score
  for that pair. This numeric score will be maximized during the parse.
  The SCORE-FN should take three arguments: left-atom, right-atom and
  the (numeric) distance between them (i.e. when the atoms are ordered
  sequentially, this is the difference between the ordinal numbers).
  If no such edge exists or is impossible to score, then minus infinity
  should be returned; such edges will not be considered. This function
  is invoked as `(SCORE-FN Atom Atom Dist)`.

  The NUM-EDGES should be an integer, indicating the number of extra
  edges to add to the GRAPH. The highest-scoring edges are added
  first, until either NUM-EDGES edges have been added, or it is not
  possible to add any more edges.  There are two reasons for not being
  able to add more edges: (1) there is no room or (2) no such edges are
  recorded in the AtomSpace (they have a score of minus-infinity). To
  add as many edges as possible, pass -1 for NUM-EDGES.

  This returns a new graph, in the form of a wedge-list.
"
	; Terminology:
	; A "numa" is a numbered atom, viz a scheme-pair (number . atom)
	; A "wedge" is a weighted edge, having the form
	;    ((left-numa . right-num) . weight).

	; Define a losing score.
	(define min-acceptable-mi -1e15)

	; Given a Left-NUMA, and a list NALI of right-numa's, return
	; a wedge-list connecting NUMA to any of the NALI's, such that
	; none of the wedges intersect an edge in the wedge-list WELI.
	(define (inter-links NUMA NALI WELI)
		(filter-map
			(lambda (r-numa)
				(define weight
					(SCORE-FN (cdr NUMA) (cdr r-numa)
						(- (car r-numa) (car NUMA))))
				(define wedge (cons (cons NUMA r-numa) weight))
				(and (< min-acceptable-mi weight)
					(not (wedge-cross-any? wedge WELI))
					wedge))
			NALI)
	)

	; Given a list NALI of numa's, return a wedge-list connecting them
	; such that none of them intersect an edge in the wedge-list WELI.
	(define (non-intersecting-links NALI WELI)
		; Tail recursive helper
		(define (*tail-rec nali rslt)
			(define rest (cdr nali))
			(if (null? rest) rslt
				(*tail-rec rest
					(append rslt (inter-links (car nali) rest WELI)))))
		(if (null? NALI) '() (*tail-rec NALI '()))
	)

	; A candidate list of links to add.
	(define candidates (non-intersecting-links NUMA-LIST GRAPH))

	; Candidates sorted by weight
	(define sorted-cands
		(sort candidates
			(lambda (sa sb)
				(< (wedge-get-score sb) (wedge-get-score sa)))))

	; Add links, one at a time, tail-recusrively.
	(define (add-link NED CANDS RSLT)
		; If we've added the requested number, we're done.
		; If there's nothing left to add, we're done.
		(if (or (= 0 NED) (null? CANDS)) RSLT
			; If the candidate edge crosses, skip it and move on.
			; else add it, and decrement the to-do count.
			(if (wedge-cross-any? (car CANDS) RSLT)
				(add-link NED (cdr CANDS) RSLT)
				(add-link (- NED 1) (cdr CANDS) (cons (car CANDS) RSLT)))))

	(add-link NUM-EDGES sorted-cands GRAPH)
)

; ---------------------------------------------------------------------

(define-public (mpg-parse-atom-seq ATOM-LIST SCORE-FN)
"
  Projective, Undirected Maximum Planar Graph parser.

  Given a sequence of atoms, find an unlabeled, undirected, projective
  maximum spanning-tree parse. To this parse, add additional edges
  until NUM-LOOPS have been created. The resulting graph is planar
  (projective) in that no edges cross.

  The ATOM-LIST should be a scheme-list of atoms, all presumably of
  a uniform atom type.

  The SCORE-FN should be a function that, when give a left-right ordered
  pair of atoms, and the distance between them, returns a numeric score
  for that pair. This numeric score will be maximized during the parse.

  See `graph-add-mpg` for additional details.
"
	; Number the atoms in sequence-order.
	(define numa-list (atom-list->numa-list ATOM-LIST))

	; Start with the MST parse
	(define mst-tree (graph-add-mst '() numa-list SCORE-FN -1))

	(graph-add-mpg mst-tree numa-list SCORE-FN -1)
)

; ---------------------------------------------------------------------
