;
; mst-parser.scm
;
; Maximum Spanning Tree parser.
;
; Copyright (c) 2014, 2017, 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The functions below accept a sequence of atoms, and create a maximum
; spanning tree (MST) graph, such that the edges of the tree maximize a
; scoring function.
;
; Input is a sequence of atoms, together with a scoring function for
; ordered pairs of atoms. In a prototypical usage, the scoring function
; will return the mutual information between a pair of atoms, and
; so the MST graph is the planar tree that connects the atoms, and
; maximizes the mutual information between pairs of atoms.
;
; The algorithm implemented is a basic maximum spanning tree algorithm.
; It can start with any existing graph, and add edges, until the desired
; number of edges has been added, or until it is impossible to add more
; edges, and still have a tree.  Edges are added only if they have a
; score that isn't minus-infinity.
;
; In the prototypical usage, the starting graph is empty, so that the
; result is a planar spanning tree. If the starting graph is not added,
; then planar trees are appended to it.
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))

; ---------------------------------------------------------------------

(define-public (make-score-fn LLOBJ METHOD)
"
  make-score-fn LLOBJ METHOD -- Create a function that returns a
  score for a pair of atoms, the score being given by invoking
  METHOD on LLOBJ.  The LLOBJ must provide the METHOD, of course,
  and also the 'get-pair method, so that pairs can be assembled.

  If either atom is nil, or if the atom-pair cannot be found, then a
  default value of -1e40 is returned.
"
	; Define a losing score.
	(define bad-mi -1e40)

	(lambda (left-atom right-atom distance)

		; We take care here to not actually create the atoms,
		; if they aren't already in the atomspace. 'get-pair returns
		; nil if the atoms can't be found.
		(define wpr
			(if (and (not (null? left-atom)) (not (null? right-atom)))
				(LLOBJ 'get-pair left-atom right-atom)
				'()))
		(if (null? wpr) bad-mi (LLOBJ METHOD wpr))
	)
)

; ---------------------------------------------------------------------

(define-public (graph-add-mst GRAPH NUMA-LIST SCORE-FN NUM-EDGES)
"
  Projective, Undirected Maximum Spanning Tree parser.

  Given an existing (possibly empty) GRAPH, extend it by adding up
  to NUM-EDGES new edges, adding them one at a time, such that each
  added edge having the highest score possible, and does not intersect
  any of the existing edges. If NUM-EDGES is set to -1, then as many
  edges as possible are added, until a planar spanning tree is created,
  or until it is impossible to add a new edge (because the edge-score
  is minus-infinity).

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
  able to add more edges: (1) the extension would no longer be a tree,
  or (2) no such edges are recorded in the AtomSpace (they have a score
  of minus-infinity). To add as many edges as possible, pass -1 for
  NUM-EDGES.

  This returns a new graph, in the form of a wedge-list.
"
	; Define a losing score.
	(define bad-mi -1e30)
	(define min-acceptable-mi -1e15)

	; Define a losing numa-pair
	(define bad-pair (cons (cons (cons 0 #f) (cons 0 #f)) bad-mi))

	; A "numa" is a numbered atom, viz a scheme-pair (number . atom)
	;
	; Given a left-numa, and a list of numas to the right of it, pick
	; an atom from the list that has the highest-weight attachment to
	; the left atom.  Return a weighted edge containing selected
	; numa-pair and it's weight.  Specifically, the given left-numa,
	; and the discovered right-numa, return the form
	; ((left-numa . right-num) . weight).
	; The search is made over atom pairs scored by the SCORE-FN.
	;
	; The left-numa is assumed to be an scheme-pair, consisting of an ID,
	; and an atom; thus the atom is the cdr of the left-numa.
	; The numa-list is likewise assumed to be a list of numbered atoms.
	;
	(define (pick-best-cost-left-pair left-numa numa-list)
		(fold
			(lambda (right-numa max-pair)
				(define max-mi (cdr max-pair))
				(define cur-mi
					(SCORE-FN (cdr left-numa) (cdr right-numa)
						(- (car right-numa) (car left-numa))))

				; Use strict inequality, so that a shorter dependency
				; length is always preferred.
				(if (< max-mi cur-mi)
					(cons (cons left-numa right-numa) cur-mi)
					max-pair
				)
			)
			bad-pair
			numa-list
		)
	)

	; Given a right-numa, and a list of numas to the left of it, pick
	; an atom from the list that has the highest-weight attachment to the
	; right atom.  Return a scheme-pair containing selected numa-pair
	; and it's weight.  Specifically, the given right-numa, and the
	; discovered left-numa, in the form ((left-numa . right-num) . mi).
	; The search is made over atom pairs scored by the SCORE-FN.
	;
	; The right-numa is assumed to be an scheme-pair, consisting of an ID,
	; and an atom; thus the atom is the cdr of the right-numa. The
	; numa-list is likewise assumed to be a list of numbered atoms.
	;
	(define (pick-best-cost-right-pair right-numa numa-list)
		(fold
			(lambda (left-numa max-pair)
				(define max-mi (cdr max-pair))
				(define cur-mi
					(SCORE-FN (cdr left-numa) (cdr right-numa)
						(- (car right-numa) (car left-numa))))

				; Use less-or-equal, so that a shorter dependency
				; length is always preferred.
				(if (<= max-mi cur-mi)
					(cons (cons left-numa right-numa) cur-mi)
					max-pair
				)
			)
			bad-pair
			numa-list
		)
	)

	; Given a list of numas, return a weighted numa-pair, in the form
	; ((left-numa . right-numa) . weight).
	;
	; The search is made over atom pairs scored by the SCORE-FN.
	;
	(define (pick-best-cost-pair numa-list)

		; scan from left-most numa to the right.
		(define best-left (pick-best-cost-left-pair
				(car numa-list) (cdr numa-list)))
		(if (eq? 2 (length numa-list))
			; If the list is two numas long, we are done.
			best-left
			; else the list is longer than two numas. Pick between two
			; possibilities -- those that start with left-most numa, and
			; something else.
			(let ((best-rest (pick-best-cost-pair (cdr numa-list))))
				(if (< (cdr best-left) (cdr best-rest))
					best-rest
					best-left
				)
			)
		)
	)

	; Create an initial graph, by finding the edge with the highest
	; weight in the graph. If there are no such edges, return the
	; empty list. This can happen if the zero function returns minus
	; infinity for each potential edge.
	(define (starting-edge numa-list)
		(define start-pair (pick-best-cost-pair numa-list))
		(if (equal? bad-pair start-pair) '() (list start-pair))
	)

	; Of multiple possibilities, pick the one with the highest weight
	; The choice-list is assumed to be a list of weighted numa-pairs,
	; each weighted edge of the form ((left-numa . right-num) . weight).
	(define (max-of-pair-list choice-list)

		; The tail-recursive helper that does all the work.
		(define (*pick-best choice-list best-so-far)
			(define so-far-mi (cdr best-so-far))
			(if (null? choice-list)
				best-so-far  ; we are done!
				(let* ((first-choice (car choice-list))
						(first-mi (cdr first-choice))
						(curr-best
							; use greater-than-or-equal; want to reject
							; bad-pair as soon as possible.
							(if (<= so-far-mi first-mi)
								first-choice
								best-so-far)))
					(*pick-best (cdr choice-list) curr-best)))
		)
		(*pick-best choice-list bad-pair)
	)

	; Given a single break-numa, return a list of connections to each
	; of the other numas in the sequence  The break-numa is assumed
	; to be ordinal-numbered, i.e. an integer, followed by an atom.
	; The numa-list is assumed to be a list of numas. It is presumed
	; that the brk-numa does NOT occur in the numa-list.
	;
	; The returned list is a list of weighted-pairs. Each weighted pair is
	; of the form ((left-numa . right-num) . weight).
	;
	; This only returns connections, if there are any. This might return
	; the empty list, if there are no connections at all.
	(define (connect-numa brk-numa numa-list)
		; The ordinal number of the break-numa.
		(define brk-num (car brk-numa))
		; The atom of the break-numa
		(define brk-node (cdr brk-numa))

		(filter-map
			(lambda (numa)
				; try-num is the ordinal number of the trial numa.
				(define try-num (car numa))
				; try-node is the actual atom of the trial numa.
				(define try-node (cdr numa))

				; Ordered pairs, the left-right order matters.
				(if (< try-num brk-num)

					; Returned value: the weight value for the pair, then the pair.
					(let ((mi (SCORE-FN try-node brk-node (- brk-num try-num))))
						(if (< min-acceptable-mi mi)
							(cons (cons numa brk-numa) mi) #f))

					(let ((mi (SCORE-FN brk-node try-node (- try-num brk-num))))
						(if (< min-acceptable-mi mi)
							(cons (cons brk-numa numa) mi) #f))
				)
			)
			numa-list
		)
	)

	; For each connected numbered-atom (numa), find connections between
	; that and the unconnected numas.  Return a list of weighted
	; connections. Each weighted-connection is of the form
	; ((left-numa . right-num) . weight).
	;
	; The 'bare-numas' is a set of the unconnected atoms, labelled by
	; an ordinal number denoting sequence order.  The graph-numas is a
	; set of numas that are already a part of the spanning tree.
	; It is assumed that these two sets have no numas in common.
	;
	; This might return an empty list, if there are no connections!
	(define (connect-to-graph bare-numas graph-numas)
		(append-map
			(lambda (grph-numa) (connect-numa grph-numa bare-numas))
			graph-numas
		)
	)

	; Find the highest-weight link that doesn't cross.
	(define (pick-no-cross-best candidates graph-wedges)
		; Despite the recursive nature of this call, we always expect
		; that best isn't nil, unless there's a bug somewhere ...
		(define best (max-of-pair-list candidates))
		(if (not (wedge-cross-any? best graph-wedges))
			best
			; Else, remove best from list, and try again.
			(pick-no-cross-best
				(lset-difference equal? candidates (list best)) graph-wedges)
		)
	)

	; Which numa of the pair is in the numa-list?
	(define (get-fresh cost-pair numa-list)
		(define numa-pair (car cost-pair)) ; throw away weight
		(define left-numa (car numa-pair))
		(define right-numa (cdr numa-pair))
		(if (any (lambda (numa) (equal? numa left-numa)) numa-list)
			left-numa
			right-numa
		)
	)

	; Find the maximum spanning tree. Tail-recursive.
	;
	; numa-list is the list of unconnected numas, to be added to the tree.
	; graph-links is a list of edges found so far, joining things together.
	; nected-numas is a list numas that are part of the tree.
	;
	; When the numa-list becomes empty, the pair-list is returned.
	;
	; The numa-list is assumed to be a set of ordinal-numbered atoms;
	; i.e. scheme-pair of an ordinal number denoting atom-order in
	; sequence, and then the atom.
	;
	; The nected-numas are likewise.  It is assumed that the numa-list and
	; the nected-numas are disjoint sets.
	;
	; The graph-links are assumed to be a set of weighted numa-pairs.
	; That is, a pair of numas followed by a floating-point weight.
	;
	(define (*pick-em numa-list graph-links nected-numas n-to-do)

		; (format #t "----------------------- \n")
		; (format #t "enter pick-em with numalist=~A\n" numa-list)
		; (format #t "and graph-links=~A\n" graph-links)
		; (format #t "and nected=~A\n" nected-words)

		; Generate a set of possible links between unconnected numas,
		; and the connected graph. This list might be empty
		(define trial-pairs (connect-to-graph numa-list nected-numas))

		; Find the best link that doesn't cross existing links.
		(define best (pick-no-cross-best trial-pairs graph-links))

		; There is no such "best link" i.e. we've never observed it
		; and so have no weight for it, then we are done.  That is, none
		; of the remaining numas can be connected to the existing graph.
		(if (or (= 0 n-to-do) (>= min-acceptable-mi (cdr best)))
			graph-links
			(let* (
					; Add the best to the list of graph-links.
					(bigger-graph (append graph-links (list best)))

					; Find the freshly-connected numa.
					(fresh-numa (get-fresh best numa-list))
					; (jd (format #t "fresh atom=~A\n" fresh-numa))

					; Remove the freshly-connected numa from the numa-list.
					(shorter-list (lset-difference equal? numa-list (list fresh-numa)))

					; Add the freshly-connected numa to the connected-list
					(more-nected (append nected-numas (list fresh-numa)))
				)

				; If numa-list is null, then we are done. Otherwise, trawl.
				(if (null? shorter-list)
					bigger-graph
					(*pick-em shorter-list bigger-graph more-nected (- n-to-do 1))
				)
			)
		)
	)

	; If no starting graph specified, then find a pair of atoms
	; connected with the largest weight in the sequence.
	(define starting-graph
		(if (null? GRAPH)
			(starting-edge NUMA-LIST)
			GRAPH))

	; Create a list of numas that are already in the graph.
	(define nected-list (numas-in-wedge-list starting-graph))

	; Create a list of numas that are not yet in the graph.
	(define discon-list (lset-difference equal? NUMA-LIST nected-list))

	; If there's no graph, and we can't figure out where to start,
	; then we are done.
	(if (null? starting-graph)
		'()
		(*pick-em discon-list starting-graph nected-list NUM-EDGES))
)

; ---------------------------------------------------------------------

(define-public (mst-parse-atom-seq ATOM-LIST SCORE-FN)
"
  Projective, Undirected Maximum Spanning Tree parser.

  Given a sequence of atoms, find an unlabeled, undirected, projective
  dependency parse of the sequence, by finding a dependency tree that
  maximizes the pair-wise scoring function. This returns a list of
  atom-pairs, together with associated score.  The tree is projective,
  in that no edges cross.

  The ATOM-LIST should be a scheme-list of atoms, all presumably of
  a uniform atom type.

  The SCORE-FN should be a function that, when give a left-right ordered
  pair of atoms, and the distance between them, returns a numeric score
  for that pair. This numeric score will be maximized during the parse.
  The most basic choice is to use the mutual information between the
  pair of atoms.

  -----
  The M in MST often stands for 'minimum'; but in this code, the
  score is maximized.

  There are many MST algorithms; the choice was made as follows:
  Prim is very easy; but seems too simple to give good results.
  Kruskal is good, but it seems hard to control a no-link-crossing
  constraint with it. This implements a variant of Borůvka's algo,
  which seems to be robust, and fast enough for the current needs.

  It has been benchmarked (using the code in `bench-mst`) to run in
  O(N^3) time, for a sequence of length N. From what I can tell, the
  state-of-the-art projective algo is Eisner, which runs at O(N^3) time.
  The code here is NOT Eisner! but seems to have comparable run-time.

  The projective (no-edge-cross) constraint might not be required, see
  R. Ferrer-i-Cancho (2006) “Why do syntactic links not cross?”
  However, that would require changing the metric from mutual information
  to something else, perhaps incorporating the dependency distance
  (as defined by Ferrer-i-Cancho), or possibly the 'hubiness', or some
  combination.  Since I really, really want to stick to entropy concepts,
  the mean-dependency-distance metric needs to be re-phrased as some
  sort of graph entropy. Hmmm...

  Another idea is to apply the Dick Hudson Word Grammar landmark
  transitivity idea, but exactly how this could work for unlabeled
  trees has not been explored.

  So, for now, a no-links-cross constraint is hand-coded into the algo.
  Without it, it seems that the pair-MI scores alone give rather unruly
  dependencies (unclear, needs exploration).  So, in the long-run, it
  might be better to instead pick something that combines MI scores with
  mean-dependency-distance or with hubiness. See, for example:
  Haitao Liu (2008) “Dependency distance as a metric of language
  comprehension difficulty” Journal of Cognitive Science, 2008 9(2): 159-191.
  or also:
  Ramon Ferrer-i-Cancho (2013) “Hubiness, length, crossings and their
  relationships in dependency trees”, ArXiv 1304.4086
"
	; Number the atoms in sequence-order.
	(define numa-list (atom-list->numa-list ATOM-LIST))

	; This is just a wrapper
	(graph-add-mst '() numa-list SCORE-FN -1)
)

; ---------------------------------------------------------------------
