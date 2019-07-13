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
; The scripts below analyze a sequence of atoms, assigning to them a
; planar-graphe (MPG) parse, showing the dependencies between the atoms
; in the sequence. The result of this parse is then used to create
; disjuncts, summarizing how the atoms in the sequence are allowed to
; connect.
;
; Input is sequence of atoms, together with a scoring function for
; ordered pairs of atoms. In the typical usage, the scoring function
; will return the mutual information between a pair of atoms, and
; so the MPG parse is a planar graph (i.e. with loops) that is maximally
; connected in such a way that the mutual information between pairs of
; atoms. is maximized.
;
; The algorithm implemented is built on top of a maximum spanning tree
; MST algorithm. Starting with the MST parse, it adds additional edges,
; those with the highest MI, until a desired number of loops is created.
; (Setting the max-loops to zero just yeilds the MST parse).
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))
(use-modules (opencog matrix))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))

; ---------------------------------------------------------------------

(define-public (mpg-parse-atom-seq ATOM-LIST SCORE-FN NUM-LOOPS)
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
  The most basic choice is to use the mutual information between the
  pair of atoms.  The SCORE-FN should take three arguments: left-atom,
  right-atom and the (numeric) distance between them (i.e. when the
  atoms are ordered sequentially, this is the difference between the
  ordinal numbers).

  The NUM-LOOPS should be an integer, indicating the number of extra
  edges to add to the MST tree. The highest-scoring edges are added
  first, until either NUM-LOOPS edges have been added, or it is not
  possible to add any more edges.  There are two reasons for not being
  able to add more edges: (1) there is no room or (2) no such edges are
  recorded in the AtomSpace. To iterate until maximum, pass -1 for
  NUM-LOOPS.
"
	; Terminology:
	; A "numa" is a numbered atom, viz a scheme-pair (number . atom)
	; A wedge" is a weighted edge, having the form
	;    ((left-numa . right-num) . weight).

	; Start with the MST parse
	(define mst-tree (mst-parse-atom-seq ATOM-LIST SCORE-FN))

	; The the list of nodes in it.
	(define node-list (sort-numalist (numas-in-wedge-list mst-tree)))

	; Define a losing score.
	(define min-acceptable-mi -1e15)

	; Given an Left-NUMA, and list NALI of right-numa's, return a
	; wedge-list connecting NUMA to any of the NALI's, such that
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
		(define (tail-rec nali rslt)
			(define rest (cdr nali))
			(if (equal? '() rest) rslt
				(tail-rec rest
					(append rslt (inter-links (car nali) rest WELI)))))
		(if (equal? '() NALI) '() (tail-rec NALI '()))
	)

	; A candidate list of links to add.
	(define candidates (non-intersecting-links node-list mst-tree))

	; Candidates sorted by weight
	(define sorted-cands
		(sort candidates
			(lambda (sa sb)
				(< (wedge-get-score sb) (wedge-get-score sa)))))

	; Add links, one at a time, tail-recusrively.
	(define (add-link NED CANDS RSLT)
		; If we've added the requested number, we're done.
		; If there's nothing left to add, we're done.
		(if (or (= 0 NED) (equal? '() CANDS)) RSLT
			; If the candidate edge crosses, skip it and move on.
			; else add it, and decrement the to-do count.
			(if (wedge-cross-any? (car CANDS) RSLT)
				(add-link NED (cdr CANDS) RSLT)
				(add-link (- NED 1) (cdr CANDS) (cons (car CANDS) RSLT)))))

	(add-link NUM-LOOPS candidates mst-tree)
)

; ---------------------------------------------------------------------
