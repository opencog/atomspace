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
; edge connecting two vertexes (one vertex is the spider-body; the other
; vertex is the tip of a leg). Its a labelled edge - the DirNode is the
; label.  Formally, the body (without the legs) is called the "germ".
;
; The utilities provide various ways of accessing different parts of
; the section, given one of the atoms in a section. So, for example:
; given a germ, find the various parts of sections:
;
; get-germ-connector-seqs - given the germ, return a list of all
;      ConnectorSeq's appearing in sections on the germ.  The
;      connector sequences are in one-to-one correspondence with
;      the sections on the germ.
;
; get-germ-connectors     - given the germ, return a list of all
;      Connectors that appear in some section on the germ.
;
; get-germ-endpoints      - given the germ, return a list of all
;      endpoints (legs or vertexes) on all sections having that germ.
;      The endpoint is defined as the first atom in the connector.
;
; Conversely, given one of the other parts, find the germs:
;
; get-conseq-germs        - given a connector sequence, return all
;       germs that have this connector sequence in their section.
;       There is one connector sequence per section.
;
; NOTES:
; ------
; This is currently implemented in just plain-old scheme, and should
; be fine for general use. However, performance could be much improved
; by re-implementing these in C++. Basically, these just do a lot of
; very simple atom accesses, and thus the overhead of guile is
; proportionately greater.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog persist))

; ---------------------------------------------------------------
;
(define-public (get-germ-sections GERM)
"
  get-germ-sections GERM - return all sections that the germ is
  at the center of.

  Assumes that the sections for the germ are already in the atomspace.
  Use `fetch-germ-sections` to load them from storage.
"
	; This is a trivial wrapper ... but hey.
	(cog-incoming-by-type GERM 'Section)
)

; ---------------------------------------------------------------
;
(define-public (fetch-germ-sections GERM)
"
  fetch-germ-sections GERM - return all sections that the germ is
  at the center of.

  Fetches the sections from storage; does not assume they are loaded
  yet.  Use `get-germ-sections` if fetching is not needed.
"
	; This is a trivial wrapper ... but hey.
	(fetch-incoming-by-type GERM 'Section)
	(cog-incoming-by-type GERM 'Section)
)

; ---------------------------------------------------------------
;
(define-public (get-germ-connector-seqs GERM)
"
  get-germ-connector-seqs GERM - return all connector sequences
  that appear in sections on the GERM. There is one connector sequence
  per section.

  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-germ-sections GERM)
"
	; Walk over all the Sections on the germ.
	; The ConnectorSeq is in position 1 in the section.
	(map
		(lambda (SEC) (cog-outgoing-atom SEC 1))
		(get-germ-sections GERM))
)

; ---------------------------------------------------------------
;
(define-public (get-germ-connectors GERM)
"
  get-germ-connectors GERM - return all connectors that appear in
  the connector sequences of (all) sections on the GERM.

  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-incoming-by-type GERM 'Section)
"
	(delete-dup-atoms
		(concatenate!
			(map cog-outgoing-set
				(get-germ-connector-seqs GERM))))
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
	; Walk over all the connectors, extracting the endpoints.
	(delete-dup-atoms
		(map
			(lambda (CNCTR) (cog-outgoing-atom CNCTR 0))
			(get-germ-connectors GERM)))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
;
(define-public (get-conseq-sections CONSEQ)
"
  get-conseq-sections CONSEQ - return all sections that have this
  connector sequence in them.

  Assumes that all sections are already in the atomspace; if not, use
  `fetch-conseq-sections` instead.
"
	; Walk over all the Sections on the connector sequence.
	; The germ is in position 0 in the section.
	(cog-incoming-by-type CONSEQ 'Section)
)

; ---------------------------------------------------------------
;
(define-public (fetch-conseq-sections CONSEQ)
"
  fetch-conseq-sections CONSEQ - return all sections that have this
  connector sequence in them.

  Fetches sections from storage (does not assume they have been loaded
  yet). Use 'get-conseq-sections` if fetching is not needed.
"
	(fetch-incoming-by-type CONSEQ 'Section)
	(get-conseq-sections CONSEQ)
)

; ---------------------------------------------------------------
;
(define-public (get-connector-sections CNCTR)
"
  get-connector-sections CONNECTOR - return all sections that have
  this connector appearing in their connector sequence.

  Assumes that all connector sequences and sections are already in
  the atomspace; if not, use `fetch-connector-sections` instead.
"
	; get-conseq-sections returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map get-conseq-sections
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)

; ---------------------------------------------------------------
;
(define-public (fetch-connector-sections CNCTR)
"
  fetch-connector-sections CONNECTOR - return all sections that have
  this connector appearing in their connector sequence.

  Fetches sections and connector sequences from storage (does not
  assume they have been loaded yet). Use 'get-connector-sections`
  if fetching is not needed.
"
	(fetch-incoming-by-type CNCTR 'ConnectorSeq)
	; fetch-conseq-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map fetch-conseq-sections
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)

; ---------------------------------------------------------------
;
(define-public (get-endpoint-sections END)
"
  get-endpoint-sections ENDPOINT - return all sections that have this
  endpoint appearing in a connector in their connector sequences.

  Assumes that all connector sequences and sections are already in
  the atomspace; if not, use `fetch-endpoint-sections` instead.
"
	; get-connector-sections returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map get-connector-sections
				(cog-incoming-by-type END 'Connector))))
)

; ---------------------------------------------------------------
;
(define-public (fetch-endpoint-sections END)
"
  fetch-endpoint-sections ENDPOINT - return all sections that have this
  endpoint appearing in a connector in a connector sequence.

  Fetches connectors, connector sequences and sections from storage
  (does not assume they have been loaded yet). Use
  'get-connector-sections` if fetching is not needed.
"
	(fetch-incoming-by-type END 'Connector)
	; fetch-connector-sections returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map fetch-connector-sections
				(cog-incoming-by-type END 'Connector))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Same as above, but return germs, not sections.
; Therefore, all of the below have an equivalent implementation
; having the general form:
;
;     (delete-dup-atoms
;        (map (lambda (SEC) (cog-outgoing-atom SEC 0))
;           (get-whatever-sections THING)))
;
; Unclear which implementation might be faster. These have not been
; tuned for performance.
;
(define-public (get-conseq-germs CONSEQ)
"
  get-conseq-germs CONSEQ - return all germs that have this connector
  sequence in their section. There is one connector sequence per section.

  Assumes that all sections are already in the atomspace; if not, use
  `fetch-conseq-germs` instead.
"
	; Walk over all the Sections on the connector sequence.
	; The germ is in position 0 in the section.
	(map (lambda (SEC) (cog-outgoing-atom SEC 0))
		(get-conseq-sections CONSEQ))
)

; ---------------------------------------------------------------
;
(define-public (fetch-conseq-germs CONSEQ)
"
  fetch-conseq-germs CONSEQ - return all germs that have this connector
  sequence in their section. There is one connector sequence per section.

  Fetches sections from storage (does not assume they have been loaded
  yet). Use 'get-conseq-germs` if fetching is not needed.
"
	(fetch-incoming-by-type CONSEQ 'Section)
	(get-conseq-germs CONSEQ)
)

; ---------------------------------------------------------------
;
(define-public (get-connector-germs CNCTR)
"
  get-connector-germs CONNECTOR - return all germs that have this
  connector appearing in their section.

  Assumes that all connector sequences and sections are already in
  the atomspace; if not, use `fetch-connector-germs` instead.
"
	; get-conseq-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map get-conseq-germs
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))

	; An alternate implementation would be:
	; (delete-dup-atoms
	; 	(map (lambda (SEC) (cog-outgoing-atom SEC 0))
	; 		(get-connector-sections CNCTR)))
)

; ---------------------------------------------------------------
;
(define-public (fetch-connector-germs CNCTR)
"
  fetch-connector-germs CONNECTOR - return all germs that have this
  connector appearing in their section.

  Fetches sections and connector sequences from storage (does not
  assume they have been loaded yet). Use 'get-connector-germs`
  if fetching is not needed.
"
	(fetch-incoming-by-type CNCTR 'ConnectorSeq)
	; fetch-conseq-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map fetch-conseq-germs
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)

; ---------------------------------------------------------------
;
(define-public (get-endpoint-germs END)
"
  get-endpoint-germs ENDPOINT - return all germs that have this
  endpoint appearing in a connector in their section.

  Assumes that all connector sequences and sections are already in
  the atomspace; if not, use `fetch-endpoint-germs` instead.
"
	; get-connector-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map get-connector-germs
				(cog-incoming-by-type END 'Connector))))
)

; ---------------------------------------------------------------
;
(define-public (fetch-endpoint-germs END)
"
  fetch-endpoint-germs ENDPOINT - return all germs that have this
  endpoint appearing in a connector in their section.

  Fetches connectors, connector sequences and sections from storage
  (does not assume they have been loaded yet). Use 'get-endpoint-germs`
  if fetching is not needed.
"
	(fetch-incoming-by-type END 'Connector)
	; fetch-connector-germs returns a list, so concatenate them.
	(delete-dup-atoms
		(concatenate!
			(map fetch-connector-germs
				(cog-incoming-by-type END 'Connector))))
)

; ---------------------------------------------------------------
