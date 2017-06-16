;
; make-section.scm
;
; Compute the sheaf sections (connector-set disjuncts), obtained from
; an MST parse of a sequence of atoms.
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
; The code here computes sections for the parse trees created by the
; MST parser.
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

(define-public (make-sections MST-PARSE)
"
  make-sections - create sections of the MST parse tree.

  Given an MST parse of a sequence, return a list of the sections of 
  the atoms in that sequence (one section per atom).

  It is the nature of MST parses that the links between the words
  have no labels: the links are of the 'any' type. We'd like to
  discover thier types, and we begin by creating sections.
  These resemble ordinary disjuncts, except that the connectors
  are replaced by the words that they connect to.

  So, for example, given the MST parse
     (mst-parse-text 'The game is played on a level playing field')
  the word 'playing' might get this connector set:

    (PseudoWordCset
       (WordNode \"playing\")
       (PseudoAnd
          (PseudoConnector
             (WordNode \"level\")
             (LgConnDirNode \"-\"))
          (PseudoConnector
             (WordNode \"field\")
             (LgConnDirNode \"+\"))))

  Grammatically-speaking, this is not a good connector, but it does
  show the general idea: that there was a link level<-->playing and
  a link playing<-->field.
"
	; Discard links with bad MI values; anything less than
	; -50 is bad. Heck, anything under minus ten...
	(define good-links (filter
		(lambda (mlink) (< -50 (mst-link-get-score mlink)))
		MST-PARSE))

	; Create a list of all of the words in the sentence.
	(define seq-list (delete-duplicates!
		(fold
			(lambda (mlnk lst)
				(cons (mst-link-get-left-numa mlnk)
					(cons (mst-link-get-right-numa mlnk) lst)))
			'()
			good-links)))

	; Return #t if word appears on the left side of mst-lnk
	(define (is-on-left-side? wrd mlnk)
		(equal? wrd (mst-link-get-left-atom mlnk)))
	(define (is-on-right-side? wrd mlnk)
		(equal? wrd (mst-link-get-right-atom mlnk)))

	; Given a word, and the mst-parse linkset, create a shorter
	; seq-list which holds only the words linked to the right.
	(define (mk-right-seqlist seq mparse)
		(define wrd (mst-numa-get-atom seq))
		(map mst-link-get-right-numa
			(filter
				(lambda (mlnk) (is-on-left-side? wrd mlnk))
				mparse)))

	(define (mk-left-seqlist seq mparse)
		(define wrd (mst-numa-get-atom seq))
		(map mst-link-get-left-numa
			(filter
				(lambda (mlnk) (is-on-right-side? wrd mlnk))
				mparse)))

	; Sort a seq-list into ascending order
	(define (sort-seqlist seq-list)
		(sort seq-list
			(lambda (sa sb)
				(< (mst-numa-get-index sa) (mst-numa-get-index sb)))))

	; Given a word, the the links, create a pseudo-disjunct
	(define (mk-pseudo seq mlist)
		(define lefts (sort-seqlist (mk-left-seqlist seq mlist)))
		(define rights (sort-seqlist (mk-right-seqlist seq mlist)))

		; Create a list of left-connectors
		(define left-cnc
			(map (lambda (sw)
					(PseudoConnector
						(mst-numa-get-atom sw)
						(LgConnDirNode "-")))
			lefts))

		(define right-cnc
			(map (lambda (sw)
					(PseudoConnector
						(mst-numa-get-atom sw)
						(LgConnDirNode "+")))
			rights))

		; return the connector-set
		(PseudoWordCset
			(mst-numa-get-atom seq)
			(PseudoAnd (append left-cnc right-cnc)))
	)

	(map
		(lambda (seq) (mk-pseudo seq MST-PARSE))
		seq-list)
)

;  ---------------------------------------------------------------------
