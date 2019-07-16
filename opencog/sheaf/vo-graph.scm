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
; The graph is weighted because each of the edges is assigned a weight.
;
; The functions below simply provide an API to access the ordering and
; the weights.
;
; Terminology:
; A "numa" is a NUMbered Atom; it is an ordered vertex. Its an atom,
;    and an integer number indicating it's ordering.
;
; An "overt" is the same thing as a numa, short for Ordered VERTex.
;
; A "wedge" is an edge, consisting of an ordered pair of numa's.
;     Note that ordering of the vertexes in the edge give that
;     edge an implicit directionality. This need NOT correspond
;     to the ordinal numbering of the vertexes. That is, an edge
;     can point from right to left or from left to right!
;
; ---------------------------------------------------------------------
;
(use-modules (opencog))

; ---------------------------------------------------------------------
; A graph consists of a list of weighted edges, each edge consisting
; of a pair of ordered atoms.
; The functions below unpack each data structure.
;
(define-public (wedge-get-score lnk)
"
  wedge-get-score lnk -- Get the score of the link (aka 'weighted edge').
"
	(cdr lnk))

(define-public (wedge-get-left-numa lnk)
"
  wedge-get-left-numa lnk -- Get the left numbered-atom (numa) in the
  link. The numa is a scheme pair of the form (number . atom)
"
	(car (car lnk)))

(define-public (wedge-get-right-numa lnk)
"
  wedge-get-right-numa lnk -- Get the right numbered-atom (numa) in the
  link. The numa is a scheme pair of the form (number . atom)
"
	(cdr (car lnk)))

(define-public (numa-get-index numa)
"
  numa-get-index numa -- Get the index number out of the numa.
"
	(car numa))

(define-public (numa-get-atom numa)
"
  numa-get-atom numa -- Get the atom from the numa.
"
	(cdr numa))

(define-public (wedge-get-left-atom lnk)
"
  wedge-get-left-atom lnk -- Get the left atom in the weighted link.
"
	(numa-get-atom (wedge-get-left-numa lnk)))

(define-public (wedge-get-right-atom lnk)
"
  wedge-get-right-atom lnk -- Get the right atom in the weighted link.
"
	(numa-get-atom (wedge-get-right-numa lnk)))

(define-public (wedge-get-left-index lnk)
"
  wedge-get-left-index lnk -- Get the index of the left atom in the link.
"
	(numa-get-index (wedge-get-left-numa lnk)))

(define-public (wedge-get-right-index lnk)
"
  wedge-get-right-index lnk -- Get the index of the right word in the link.
"
	(numa-get-index (wedge-get-right-numa lnk)))

(define-public (numa-on-left-side? NUMA WEDGE)
"
  numa-on-left-side? NUMA WEDGE -- return #t if NUMA is on the left.

  Return #t if NUMA appears on the left side of the WEDGE.
"
	(and (not (null? WEDGE))
		(equal? NUMA (wedge-get-left-numa WEDGE)))
)

(define-public (numa-on-right-side? NUMA WEDGE)
"
  numa-on-right-side? NUMA WEDGE -- return #t if NUMA is on the right.

  Return #t if NUMA appears on the right side of the WEDGE.
"
	(and (not (null? WEDGE))
		(equal? NUMA (wedge-get-right-numa WEDGE)))
)

; ---------------------------------------------------------------------

(define-public (print-wedglist WELI)
"
  print-wedgelist WEDGE-LIST -- print the WEDGE-LIST in readable form

  Debug utility: print the WEDGE-LIST in an easy-to-read form.
  WEDGE-LIST must be a list of wedges. Assumes the Atoms are Nodes.
  XXX That should be fixed...
"
	(for-each
		(lambda (LINK)
			(format #t "~D-~D\t ~A <--> ~A\t Score=~6F\n"
				(caaar LINK) (cadar LINK)
				(cog-name (cdaar LINK)) (cog-name (cddar LINK))
				(cdr LINK)
			))
		WELI)
)

; ---------------------------------------------------------------------

(define-public (atom-list->numa-list ATOM-LIST)
"
  atom-list->numa-list ATOM-LIST -- Return an ordinal-numbered list.

  Given a list of atoms, create a numbered list of atoms. The numbering
  establishes an order for the list, and provides a unique ID, needed
  both of which are needed for the graph algos.  If the same Atom
  atom appears twice in a sequence, the ordinal distinguishes these
  multiple occurrences.
"
	(define cnt 0)
	(map
		(lambda (ato) (set! cnt (+ cnt 1)) (cons cnt ato))
		ATOM-LIST)
)


(define-public (sort-numalist NUMA-LIST)
"
  sort-numalist NUMA-LIST -- Sort a list of numas into ascending order.
"
	(sort NUMA-LIST
		(lambda (sa sb)
			(< (numa-get-index sa) (numa-get-index sb)))))

(define-public (sort-wedgelist WEDGE-LIST)
"
  sort-wedgelist WEDGE-LIST -- Sort a list of wedges into ascending order.
"
	(sort WEDGE-LIST
		(lambda (wea web)
			(define nlea (wedge-get-left-index wea))
			(define nleb (wedge-get-left-index web))
			(or (< nlea nleb)
				(and (= nlea nleb)
					(< (wedge-get-right-index wea)
						(wedge-get-right-index web))))))
)

; ---------------------------------------------------------------------

(define-public (numas-in-wedge-list WELI)
"
  numas-in-wedge-list WELI -- Create a list of all of the numas
  that appear in the wedge-list WELI. The list is de-duplicated;
  every numa appears only once.
"
	(delete-duplicates!
	(fold
		(lambda (mlnk lst)
			(cons (wedge-get-left-numa mlnk)
				(cons (wedge-get-right-numa mlnk) lst)))
		'()
		WELI))
)

(define-public (right-linked-numas NUMA WELI)
"
  right-linked-numas NUMA WELI -- return numas linked to the right.

  Given a numbered-atom NUMA, and the list of wedges WELI,
  create a list numas which holds only the numbered atoms
  linked to the right of NUMA.
"
	(map wedge-get-right-numa
		(filter
			(lambda (wedge) (numa-on-left-side? NUMA wedge))
			WELI)))

(define-public (left-linked-numas NUMA WELI)
"
  left-linked-numas NUMA WELI -- return numas linked to the left.

  Given a numbered-atom NUMA, and the list of wedges WELI,
  create a list numas which holds only the numbered atoms
  linked to the left of NUMA.
"
	(map wedge-get-left-numa
		(filter
			(lambda (wedge) (numa-on-right-side? NUMA wedge))
			WELI)))

; ---------------------------------------------------------------------

(define-public (left-most-numa NUMA WELI)
"
  left-most-numa NUMA WELI - Return the left-most numa linked to NUMA

  Return the left-most numa that can be linked to NUMA using the
  edges in WELI. This walks the WELI graph, looking for the left-most
  end of it.
"
	(define (*more-left wedge vert graph)

		; Exhaust the graph, if possible.
		(define most-here
			(if (null? graph) vert
				; Else try the rest of the graph...
				(let ((more-here
						(*more-left (car graph) vert (cdr graph))))
					(if (< (numa-get-index vert) (numa-get-index more-here))
						vert more-here))))

		; Follow the edge, if possible.
		(define linked-vert
			(cond
				((numa-on-left-side? vert wedge)
					(wedge-get-left-numa wedge))
				((numa-on-left-side? vert wedge)
					(wedge-get-left-numa wedge))
				(else '())))

		; If no linked vertex, we are done.
		(if (null? linked-vert) most-here
			; Else try the rest of the graph...
			(let ((maybe-there
					(*more-left '() linked-vert graph)))
				(if (< (numa-get-index most-here) (numa-get-index maybe-there))
					most-here maybe-there))))


	(if (null? WELI) NUMA
		(*more-left (car WELI) NUMA (cdr WELI)))
)

(define-public (right-most-numa NUMA WELI)
"
  right-most-numa NUMA WELI - Return the right-most numa linked to NUMA

  Return the right-most numa that can be linked to NUMA using the
  edges in WELI. This walks the WELI graph, looking for the right-most
  end of it.
"
	(define (*more-right wedge vert graph)

		; Exhaust the graph, if possible.
		(define most-here
			(if (null? graph) vert
				; Else try the rest of the graph...
				(let ((more-here
						(*more-right (car graph) vert (cdr graph))))
					(if (< (numa-get-index vert) (numa-get-index more-here))
						more-here vert))))

		; Follow the edge, if possible.
		(define linked-vert
			(cond
				((numa-on-right-side? vert wedge)
					(wedge-get-left-numa wedge))
				((numa-on-left-side? vert wedge)
					(wedge-get-right-numa wedge))
				(else '())))

		; If no linked vertex, we are done.
		(if (null? linked-vert) most-here
			; Else try the rest of the graph...
			(let ((maybe-there
					(*more-right '() linked-vert graph)))
				(if (< (numa-get-index most-here) (numa-get-index maybe-there))
					maybe-there most-here))))

	(if (null? WELI) NUMA
		(*more-right (car WELI) NUMA (cdr WELI)))
)

; ---------------------------------------------------------------------

(define-public (wedge-cross? wedge-a wedge-b)
"
  wedge-cross? wedge-a wedge-b Do a pair of links cross each other?

  Return true if a pair of weighted edges cross, else return false.
  If wedge-a and wedge-b define the same edge, they are considered
  to be crossing.
  Useful for constructing planar graphs.
"
	(define pair-a (car wedge-a)) ; throw away weight
	(define pair-b (car wedge-b)) ; throw away weight
	(define lwa (car pair-a)) ; left  numa of numa-pair
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
		(and (= ila ilb) (= ira irb))
	)
)

(define-public (wedge-cross-any? wedge wedge-list)
"
  wedge-cross-any? wedge wedge-list -- does this link cross any others?

  Return true if the wedge crosses over any wedges in the wedge-list.
"
	(any (lambda (pr) (wedge-cross? pr wedge)) wedge-list)
)

; ---------------------------------------------------------------------
