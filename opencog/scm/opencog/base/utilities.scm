;
; utilities.scm
;
;;; Commentary:
;
; Miscellaneous handy utilities for working with atoms.
;
; Utilities include:
; -- simple traversal of outgoing set (gar, gdr, etc.)
; -- extract-hypergraph -- extract a hypergraph and everything "under" it.
; -- extract-type -- extract all atoms of type 'atom-type'.
; -- cog-report-counts -- Return an association list of counts.
; -- count-all -- Return the total number of atoms in the atomspace.
; -- cog-get-atoms -- Return a list of all atoms of type 'atom-type'
; -- cog-get-all-roots -- Return the list of all root atoms.
; -- cog-prt-atomspace -- Prints all atoms in the atomspace
; -- cog-get-root -- Return all hypergraph roots containing 'atom'
; -- cog-get-trunk -- Return all hypergraphs containing `ATOM`.
; -- cog-get-all-subtypes -- Call recursively cog-get-subtypes
;
; Backwards-ompat wrappers. Deprecated; do not use in new code.
; -- cog-arity -- size of atoms.
;
;;; Code:
; Copyright (c) 2008, 2013, 2014 Linas Vepstas <linasvepstas@gmail.com>
;

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))  ; Needed for define*-public

; -----------------------------------------------------------------------
; Analogs of car, cdr, etc. but for atoms.
; (define (gar x) (if (cog-atom? x) (car (cog-outgoing-set x)) (car x)))
; (define (gdr x) (if (cog-atom? x) (cadr (cog-outgoing-set x)) (cdr x)))

(define-public (gar LINK)
"
  gar LINK - return first element of a Link atom.
  Return null if the LINK is empty.
"
	(cog-outgoing-atom LINK 0)
)

(define-public (gdr LINK)
"
  gdr LINK - return second element of a Link atom.
  Return null if the LINK is empty or has only one element.
"
	(cog-outgoing-atom LINK 1)
)

(define-public (gaar x) (gar (gar x)) )
(define-public (gadr x) (gar (gdr x)) )
(define-public (gdar x) (gdr (gar x)) )
(define-public (gddr x) (gdr (gdr x)) )

(define-public (gaaar x) (gar (gar (gar x))) )
(define-public (gaadr x) (gar (gar (gdr x))) )
(define-public (gadar x) (gar (gdr (gar x))) )
(define-public (gaddr x) (gar (gdr (gdr x))) )

(define-public (gdaar x) (gdr (gar (gar x))) )
(define-public (gdadr x) (gdr (gar (gdr x))) )
(define-public (gddar x) (gdr (gdr (gar x))) )
(define-public (gdddr x) (gdr (gdr (gdr x))) )

; --------------------------------------------------------------------
(define-public (extract-hypergraph atom)
"
  extract-hypergraph -- extract a hypergraph and everything under it

  If the indicated atom has no incoming links, then extract it. Repeat
  recursively downwards, following the *outgoing* set of any links
  encountered.  This only removes the atoms from the atomspace, it
  does NOT remove it from the backingstore, if attached!
"
	(if (cog-atom? atom)     ; Make sure that atom is valid, as it may
	                         ; already have been extracted by an outer
	                         ; recursive call
		(if (cog-node? atom)
			(cog-extract! atom)
			(let* ((oset (cog-outgoing-set atom))
					(flg (cog-extract! atom))
				)
				(if flg ;; halt recursion if link was not extract-able
					(for-each extract-hypergraph oset)
				)
			)
		)
	)
)

; --------------------------------------------------------------------
(define*-public (extract-type atom-type #:optional (ATOMSPACE (cog-atomspace)))
"
  extract-type ATOM-TYPE [ATOMSPACE] -- extract all atoms of type 'atom-type'

  If any atoms of that type have incoming links, those links will be
  extracted, and so on recursively.  This only removes the atoms from the
  atomspace, it does NOT remove it from the backingstore, if attached!

  If the optional argument ATOMSPACE is provided, then the atoms are
  extracted from it; otherwise the default atomspace is used.
"
	(cog-map-type
		(lambda (x) (cog-extract-recursive! x) #f)
		atom-type
		ATOMSPACE
	)
)

; --------------------------------------------------------------------
(define*-public (cog-report-counts #:optional (ATOMSPACE (cog-atomspace)))
"
  cog-report-counts [ATOMSPACE]-- Return an association list of counts

  Return an association list holding a report of the number of atoms
  of each type currently in the ATOMSPACE. Counts are included only
  for types with non-zero atom counts.

  The argument ATOMSPACE is optional; if absent, the current atomspace
  is used.

  See also:
     cog-count-atoms -- which counts atoms of a given type.
     count-all -- report a single grand-total count.
"
	(let ((tlist (cog-get-types)))
		(define (rpt type)
			(let ((cnt (cog-count-atoms type ATOMSPACE)))
				(if (not (= 0 cnt))
					(cons type cnt)
					#f
				)
			)
		)
		(filter-map rpt tlist)
	)
)
; --------------------------------------------------------------------
(define*-public (count-all #:optional (ATOMSPACE (cog-atomspace)))
"
  count-all [ATOMSPACE] -- Return the total number of atoms in ATOMSPACE

  If the optional argument ATOMSPACE is not supplied, the current
  atomspace is used.

  This does NOT count atoms in the backing store!

  See also:
     cog-count-atoms -- which counts atoms of a given type.
     cog-report-counts -- which reports counts by type.
"
	(fold (lambda (typ cnt) (+ cnt (cog-count-atoms typ ATOMSPACE)))
		0 (cog-get-types))
)

; -----------------------------------------------------------------------
(define-public (cog-get-atoms atom-type . subtypes)
"
  cog-get-atoms TYPE [BOOL] -- Return list of all atoms of TYPE,
                   including subtypes if BOOL is #t.

  Return a list of all atoms in the atomspace that are of type
  TYPE. If the optional argument BOOL is provided and set to #t,
  then all atoms of the subtypes of TYPE are returned as
  well.  Otherwise, only atoms of type TYPE are returned.

  Examples:

     (display (cog-get-atoms 'ConceptNode))
  will return and display all atoms of type 'ConceptNode

     (display (cog-get-atoms 'Atom #t))
  will return and display all atoms in the AtomSpace.

  See also:
     cog-get-all-roots -- get only the roots.
     cog-count-atoms -- count atoms of a given type.
     cog-report-counts -- provide a report of the different atom types.
"
	(let ((lst '()))
		(define (mklist atom)
			(set! lst (cons atom lst))
			#f
		)
		(if (and (not (null? subtypes)) (eq? (car subtypes) #t))
			(for-each (lambda (x) (cog-map-type mklist x))
				(cons atom-type (cog-get-all-subtypes atom-type)))
			(cog-map-type mklist atom-type))
		lst
	)
)

; -----------------------------------------------------------------------
(define*-public (cog-prt-atomspace #:optional (ATOMSPACE (cog-atomspace)))
"
  cog-prt-atomspace [ATOMSPACE] -- Prints all atoms in the atomspace

  This will print all of the atoms in the (optional argument) ATOMSPACE,
  or in the default atomspace, if the optional argument is absent.
  It prints only those atoms that have no incoming set (in the
  atomspace, or its ancestors), and thus are at the top of a tree.
  All other atoms (those which do have an incoming set) will appear
  somewhere underneath these top-most atoms.

  This is equivalent to `(display (cog-get-all-roots))`.

  To dump large atomspaces to a file, use `FileStorageNode`. For example:
     (use-modules (opencog persist-file))
     (define fsn (FileStorageNode \"/tmp/bigspace.scm\"))
     (cog-open fsn)
     (store-atomspace fsn)
     (cog-close fsn)
"
	(define (prt-atom h)
		; Print only the top-level atoms.
		(if (null? (cog-incoming-set h ATOMSPACE))
			(display h))
		#f)

	(for-each
		(lambda (ty) (cog-map-type prt-atom ty ATOMSPACE))
		(cog-get-all-subtypes 'Atom))
)

; -----------------------------------------------------------------------
(define*-public (cog-get-all-roots #:optional (ATOMSPACE (cog-atomspace)))
"
  cog-get-all-roots [ATOMSPACE] -- Return the list of all root atoms.

  Return the list of all root atoms in the (optional argument) ATOMSPACE,
  or in the default atomspace, if the optional argument is absent.
  It returns only those atoms that have no incoming set (in the
  atomspace, or its ancestors), and thus are at the top of a tree.
  All other atoms (those which do have an incoming set) will appear
  somewhere underneath these top-most atoms.

  See also: cog-get-atoms, cog-get-root
"
	(define roots '())
	(define (cons-roots x)
		(if (null? (cog-incoming-set x ATOMSPACE))
			(set! roots (cons x roots)))
		#f)
	(for-each
		(lambda (ty) (cog-map-type cons-roots ty ATOMSPACE))
		(cog-get-types))
	roots
)

; -----------------------------------------------------------------------
(define-public (cog-get-root ATOM)
"
  cog-get-root -- Return all hypergraph roots containing `ATOM`.

  Return all links that contain ATOM and are also roots. A root
  is any atom that has a null incoming set.

  See also: cog-get-all-roots, cog-get-trunk
"
	(define iset (cog-incoming-set ATOM))
	(if (null? iset)
		(list ATOM)
		(append-map cog-get-root iset))
)

; -----------------------------------------------------------------------
(define-public (cog-get-trunk ATOM)
"
  cog-get-trunk -- Return all hypergraphs containing `ATOM`.

  Return all links that contain ATOM at some level.  Unlike
  `cog-get-root`, these are not necessarily roots.
"
	(define iset (cog-incoming-set ATOM))
	(if (null? iset)
		'()
		(concatenate (list iset
			(append-map cog-get-trunk iset))))
)

; -----------------------------------------------------------------------
(define-public (cog-get-all-subtypes atom-type)
"
 cog-get-all-subtypes TYPE
    Given an atom type TYPE, return all its subtypes, direct
    and indirect, via recursively calling cog-get-subtypes.
"
  (let* ((subtypes (cog-get-subtypes atom-type))
         (rec-subtypes (map cog-get-all-subtypes subtypes)))
    (delete-duplicates (append subtypes (apply append rec-subtypes)))))

; ---------------------------------------------------------------------
(define-public (cog-arity ITEM)
"
 cog-arity VALUE
    Return the size of VALUE (an Atom or Value)

    Example:
       guile> (define x (Concept \"abc\"))
       guile> (cog-arity x)
       1
       guile> (define l (Link x x x))
       guile> (cog-arity l)
       3
"
	(length (cog-value->list ITEM))
)
; ---------------------------------------------------------------------
