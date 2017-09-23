;
; make-section.scm
;
; Compute the sheaf sections (connector-set disjuncts), given a set of
; edges connecting ordered vertexes.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The topological structure of a graph can be understood locally in
; terms of "sheaf theory". In this framework, instead of looking at
; a graph as whole, one instead looks at it locally, in terms of how
; any given vertex attaches to the other vertexes around it. Each
; such set of attachments is a "local section" of a sheaf,
; characterizing how the vertex can attach to a graph.
;
; An example of a section is
;
;    Section
;        Atom "A"
;        ConnectorSeq
;            Connector
;                Atom "B"
;                Label "A-to-B label"
;            Connector
;                Atom "C"
;                Label "A-to-C label"
;
; which indicates that A is connected to B and to C; viz, that there are
; edges (AB) and (AC).  Note that the ConnectorSeq is independent of the
; atom "A".  This allows different sections to be compared: e.g. there
; may be a different atom D with edges (DB) and (DC).  Then D would have
; the same ConnectorSeq as A. Thus, the ConnectorSeq simplifies the
; discovery of subgraph isomorphisms.
;
; The ConnecorSeq is an ordered link; it is presumed that, in the
; general case, that the order of the connectors matter.
;
; In the above example, the edges carry (optional) edge labels
; indicating the type of the connector.
;
; In everything that follows, it is assumed that all of the vertexes of
; the graph are sequentially ordered, i.e. can be laid out in sequence
; from left to right.  In such a case, each connector can also be
; endowed with a direction, indicating if the connected atom lies to the
; left or to the right.  The ConnectorDir thus is used to preserve
; ordering information, while still being flexible enough to help with
; the subgraph isomorphism problem.
;
; An example of a section with ordered vertexes is
;
;    Section
;        Atom "something"
;        ConnectorSeq
;            Connector
;                Atom "it's"
;                ConnectorDir "-"
;            Connector
;                Atom "curious"
;                ConnectorDir "+"
;
; which captures the idea that a sequence of atoms was observed:
; "it's something curious", which was subsequently MST-parsed as
; "it's" <--> "something" <--> "curious".  The middle vertex,
; "something", has degree two, as it has edges to the left and to the
; right. Thus, the local shape of the graph is that the vertex
; "something" connects to two other vertexes, explicitly named in the
; local section.
;
; The code here computes sections for graphs consisting of ordered
; vertexes.  The parse trees created by the MST parser are of this type.
;
; After a sequence of atoms has been parsed with the MST parser, the
; links between atoms in the parse can be interpreted as Link Grammar
; links (connector pairs).  The connector pair is the labelled edge
; between the two atoms; the label itself is is given by the names of
; the two endpoints. A single connector is then just a direction (to
; the left, to the right) plus the vertex atom at the far end.
;
; The section (aka connector set) is then a sequence of conectors; the
; number of connectors in the section exactly equal to the degree of
; the vertex in the MST parse: the connector set "describes" the parse
; tree, locally.
;
; In the current implementation, the ConnectorDir can be either "-" or
; "+" indicating whether the connection is to the left or the right.
; Other values are possible, in principle, including a "don't-care"
; directional relationship, as well as an indication of a head-dependent
; relationship, a distance measure, relations other than left/right
; (e.g. up/down, scissors/paper/rock), etc. 
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define-public (make-sections WEDGE-LIST)
"
  make-sections - create sections of a graph.

  Given a graph, expressed as a set of edges, return a list of sections
  for the vertexes in the graph. By definition, there is only one
  section per vertex.

  The WEDGE-LIST is assumed to be a list of weighted edges. The weights
  are ignored.  The vertexes in the graph are assumed to just be the
  set of vertexes aht appear at the ends of each edge; these are
  extracted automatically, below.  XXX FIXME a better API could just
  pass these in...

  The returned sections are a list of SectionLinks, one for each vertex.
  The SectionLink will list (in order) a list of ConnectorLink's, with
  each connector implicitly specifying an edge, by specifying the atom
  at the far end of the edge.  The connectors are lablled with direction
  marks, '+' and '-', indicating whether the far end is to the right or
  the left of the given vertex.

  So, for example, given the MST parse
     (mst-parse-text 'The game is played on a level playing field')
  the word 'playing' might get this connector set:

    (Section
       (WordNode \"playing\")
       (ConnectorSeq
          (Connector
             (WordNode \"level\")
             (ConnectorDir \"-\"))
          (Connector
             (WordNode \"field\")
             (ConnectorDir \"+\"))))

  As the local section of a single graph, it captures the local
  structure that there was a link level<-->playing and a link
  playing<-->field. The ConnectorDir indicates whether the link went
  to the left or to the right.  This allow the ConnectorSeq to be
  independent of Section itself; that is, the ConnectorSeq never
  mentions the (WordNode \"playing\").  This allows different
  ConnectorSeq's to be explicitly compared.
"
	; Discard links with bad MI values; anything less than
	; -1e6 is bad. Heck, anything under minus ten is bad...
	(define good-links (filter
		(lambda (mlink) (< -1e6 (wedge-get-score mlink)))
		WEDGE-LIST))

	; Create a list of all of the atoms in the sequence.
	(define seq-list (delete-duplicates!
		(fold
			(lambda (mlnk lst)
				(cons (wedge-get-left-overt mlnk)
					(cons (wedge-get-right-overt mlnk) lst)))
			'()
			good-links)))

	; Return #t if word appears on the left side of mst-lnk
	(define (is-on-left-side? wrd mlnk)
		(equal? wrd (wedge-get-left-atom mlnk)))
	(define (is-on-right-side? wrd mlnk)
		(equal? wrd (wedge-get-right-atom mlnk)))

	; Given a word, and the mst-parse linkset, create a shorter
	; seq-list which holds only the words linked to the right.
	(define (mk-right-seqlist seq mparse)
		(define wrd (overt-get-atom seq))
		(map wedge-get-right-overt
			(filter
				(lambda (mlnk) (is-on-left-side? wrd mlnk))
				mparse)))

	(define (mk-left-seqlist seq mparse)
		(define wrd (overt-get-atom seq))
		(map wedge-get-left-overt
			(filter
				(lambda (mlnk) (is-on-right-side? wrd mlnk))
				mparse)))

	; Sort a seq-list into ascending order
	(define (sort-seqlist seq-list)
		(sort seq-list
			(lambda (sa sb)
				(< (overt-get-index sa) (overt-get-index sb)))))

	; Given an atom, the the links, create a section
	(define (mk-pseudo seq mlist)
		(define lefts (sort-seqlist (mk-left-seqlist seq mlist)))
		(define rights (sort-seqlist (mk-right-seqlist seq mlist)))

		; Create a list of left-connectors
		(define left-cnc
			(map (lambda (sw)
					(Connector
						(overt-get-atom sw)
						(ConnectorDir "-")))
			lefts))

		(define right-cnc
			(map (lambda (sw)
					(Connector
						(overt-get-atom sw)
						(ConnectorDir "+")))
			rights))

		; return the connector-set
		(Section
			(overt-get-atom seq)
			(ConnectorSeq (append left-cnc right-cnc)))
	)

	(map
		(lambda (seq) (mk-pseudo seq WEDGE-LIST))
		seq-list)
)

;  ---------------------------------------------------------------------
