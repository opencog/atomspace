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
;                Label "go-to-B label"
;            Connector
;                Atom "C"
;                Label "go-to-C label"
;
; which indicates that A is connected to B and to C; viz, that there are
; edges (AB) and (AC).  Note that the ConnectorSeq is independent of the
; atom "A".  This allows different sections to be compared: e.g. there
; may be a different atom D with edges (DB) and (DC).  Then D would have
; the same ConnectorSeq as A. Thus, the ConnectorSeq simplifies the
; discovery of subgraph isomorphisms.
;
; In order to handle the general case, the ConnectorSeq is an ordered
; link; it is presumed that, in the general case, that the order of the
; connectors matter. (Whether the order matters, or not, depends on the
; type of graph being considered. For example, for graphs that represent
; temporal sequences, the order matters.)
;
; In the above example, the edges carry (optional) edge labels
; indicating the type of the connector. For example, the edge labels are
; handy for indicating temporal order, e.g. whether Atom B came before
; A or after A.
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
; The section (aka connector set) is then a sequence of connectors; the
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
  set of vertexes that appear at the ends of each edge; these are
  extracted automatically, below.

  The returned sections are a list of SectionLinks, one for each vertex.
  The SectionLink will list (in order) a list of ConnectorLink's, with
  each connector implicitly specifying an edge, by specifying the atom
  at the far end of the edge.  The connectors are labeled with direction
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
	; Terminology: a "numa" is a NUMbered Atom, its a pair
	; (cons integer Atom).
	; A "wedge" is a weighted edge: (cons (list lnuma rnuma) float)

	; Given a numa, and a list of wedges, create a Section
	(define (mk-pseudo NUMA WEDLI)
		(define left-nus (sort-numalist (left-linked-numas NUMA WEDLI)))
		(define right-nus (sort-numalist (right-linked-numas NUMA WEDLI)))

		; Create a list of left-connectors
		(define left-cnc
			(map (lambda (sw)
					(Connector
						(numa-get-atom sw)
						(ConnectorDir "-")))
			left-nus))

		(define right-cnc
			(map (lambda (sw)
					(Connector
						(numa-get-atom sw)
						(ConnectorDir "+")))
			right-nus))

		; return the connector-set
		(Section
			(numa-get-atom NUMA)
			(ConnectorSeq (append left-cnc right-cnc)))
	)

	(map
		(lambda (seq) (mk-pseudo seq WEDGE-LIST))
		(numas-in-wedge-list WEDGE-LIST))
)

;  ---------------------------------------------------------------------
