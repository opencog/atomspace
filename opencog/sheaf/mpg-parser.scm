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
  recorded in the AtomSpace.
"
	; Terminology:
	; A "numa" is a numbered atom, viz a scheme-pair (number . atom)
	; A wedge" is a weighted edge, having the form
	;    ((left-numa . right-num) . mi).

	; Start with the MST parse
	(define mst-tree (mst-parse-atom-seq ATOM-LIST SCORE-FN)

	; Sort it into ascending sequential order, first by left-numas
	; when they are unequal, then by right-numas, if the lefts are
	; equal.
	(define ordered-tree
		(sort mst-tree
			(lambda (wea web)
				(define nlea (wedge-get-left-index wea))
				(define nleb (wedge-get-left-index web))
				(or (< nlea nleb)
					(and (= nlea nleb)
						(< (wedge-get-right-index wea)
							(wedge-get-right-index web)))))))

xxxxxxxxxx
	; Define a losing score.
	(define bad-mi -1e30)

	; Return true if a pair of links cross, else return false.
	(define (cross? cost-pair-a cost-pair-b)
		(define pair-a (car cost-pair-a)) ; throw away MI
		(define pair-b (car cost-pair-b)) ; throw away MI
		(define lwa (car pair-a))  ; left numa of numa-pair
		(define rwa (cdr pair-a)) ; right numa of numa-pair
		(define lwb (car pair-b))
		(define rwb (cdr pair-b))
		(define ila (car lwa))     ; ordinal number of the atom
		(define ira (car rwa))
		(define ilb (car lwb))
		(define irb (car rwb))
		(or
			; All inequalities are strict.
			(and (< ila ilb) (< ilb ira) (< ira irb))
			(and (< ilb ila) (< ila irb) (< irb ira))
		)
	)

	; Return true if the pair crosses over any pairs in the pair-list
	(define (cross-any? cost-pair cost-pair-list)
		(any (lambda (pr) (cross? pr cost-pair)) cost-pair-list)
	)

	; Find the highest-MI link that doesn't cross.
	(define (pick-no-cross-best candidates graph-pairs)
		; Despite the recursive nature of this call, we always expect
		; that best isn't nil, unless there's a bug somewhere ...
		(define best (max-of-pair-list candidates))
		(if (not (cross-any? best graph-pairs))
			best
			; Else, remove best from list, and try again.
			(pick-no-cross-best
				(set-sub candidates (list best)) graph-pairs)
		)
	)
)

; ---------------------------------------------------------------------
