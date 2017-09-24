;
; sections.scm
;
; Assorted utilities for working with sections.
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Recall what a section looks like:
;
;     Section
;         Atom "foo" ; usually a node of some kind.
;         ConnectorSeq
;             Connector
;                Atom "bar" ; for example, a WordNode.
;                DirNode "+" ; Some edge label.
;             Connector
;                ....
;
; It should be thought of as being shaped like a spider, with a body
; at the center, and a bunch of legs. In the above, the body is the
; atom "foo", and "bar" is one of the legs.  Or rather, "bar" is at
; the end of one of the legs, so that foo-bar can be though of as an
; edge connecting two vertexes. Its a labelled edge - the
; DirNode is the label.  Formally, the body is called the "germ".
;
; The utilities here include:
;
; get-germ-connector-seqs - given the germ, return a list of all
;      ConnectorSeq's appearing in sections on the germ.  The
;      connector sequences are in one-to-one correspondance with
;      the sections on the germ.
;
; get-germ-connectors     - given the germ, return a list of all
;      Connectors that appear in seme section on the germ.
;
; get-germ-endpoints      - given the germ, return a list of all
;      endpoints (legs or vertexes) on all sections having that germ.
;      The endpoint is defined as the first atom in the connector.
;
; get-conseq-germs        - given a connector sequence, return all
;       germs that have this connector sequence in thier section.
;       There is one connector sequence per section.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog persist))

; ---------------------------------------------------------------
;
(define-public (get-germ-connector-seqs GERM)
"
  get-germ-connector-seqs GERM - return all connector seqeucences
  that appear in sections on the GERM. There is one connector sequence
  per section.

  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-incoming-by-type GERM 'Section)
"
	; Walk over all the Sections on the germ.
	; The ConnectorSeq is in position 1 in the section.
	(map (lambda (SEC) (cog-outgoing-atom SEC 1))
		(cog-incoming-by-type GERM 'Section))
)

; ---------------------------------------------------------------
;
(define-public (get-germ-connectors GERM)
"
  get-germ-connectors GERM - return all connectors that appear in
  the connector sequences of sections on the GERM.

  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-incoming-by-type GERM 'Section)
"
	; Given a Section i.e. a (germ, connector-set) pair, walk over
	; all the connectors in the connector set, and add the connector
	; to the connector-list.
	(define (add-to-list SEQ CNTR-LIST)
		(fold
			(lambda (CNCTR LST)
				; Hmm. Is this test for the type really needed?
				(if (eq? 'Connector (cog-type CNCTR))
					(cons CNCTR LST) LST))
			CNTR-LIST
			; second atom of Section is a ConnectorSeq
			(cog-outgoing-set SEQ)))

	; Walk over all the Sections on the germ.
	(delete-dup-atoms
		(fold add-to-list '() (get-germ-connector-seqs GERM)))
)

; ---------------------------------------------------------------
;
(define-public (get-germ-endpoints GERM)
"
  get-germ-endpoints GERM - return all vertexes that appear as
  endpoints in the connector sets on the GERM.

  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-incoming-by-type GERM 'Section)
"
	; Walk over all the connectors, extracting the enpoints.
	(delete-dup-atoms
		(map
			(lambda (CNCTR) (cog-outgoing-atom CNCTR 0))
			(get-germ-connectors GERM)))
)

; ---------------------------------------------------------------
;
(define-public (get-conseq-germs CONSEQ)
"
  get-conseq-germs CONSEQ - return all germs that have this connector
  sequence in thier section. There is one connector sequence per section.
"
	; Walk over all the Sections on the connector sequence.
	; The germ is in position 0 in the section.
	(map (lambda (SEC) (cog-outgoing-atom SEC 0))
		(cog-incoming-by-type CONSEQ 'Section))
)

; ---------------------------------------------------------------
;
(define-public (get-connector-germs CNCTR)
"
  get-connector-germs CONNECTOR - return all germs that have this
  connector appearing in thier section.
"
	; get-conseq-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map get-conseq-germs
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)

; ---------------------------------------------------------------
;
(define-public (get-endpoint-germs END)
"
  get-endpoing-germs ENDPOINT - return all germs that have this
  endpoint appearing in a connector in thier section.
"
	; get-connector-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map get-connector-germs
				(cog-incoming-by-type END 'Connector))))
)

; ---------------------------------------------------------------
