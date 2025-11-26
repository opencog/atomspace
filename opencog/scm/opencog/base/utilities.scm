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
; -- clear -- extract all atoms in the atomspace.
; -- cog-report-counts -- Return an association list of counts.
; -- count-all -- Return the total number of atoms in the atomspace.
; -- cog-get-atoms -- Return a list of all atoms of type 'atom-type'
; -- cog-get-all-roots -- Return the list of all root atoms.
; -- cog-prt-atomspace -- Prints all atoms in the atomspace
; -- cog-get-root -- Return all hypergraph roots containing 'atom'
; -- cog-get-trunk -- Return all hypergraphs containing `ATOM`.
; -- max-element-by-key -- Get maximum element in a list
; -- min-element-by-key -- Get maximum element in a list
; -- cog-push-atomspace -- Create a temporary atomspace.
; -- cog-pop-atomspace -- Delete a temporary atomspace.
; -- random-string -- Generate a random string of given length.
; -- random-node-name  -- Generate a random name for a node of given type.
; -- random-node  -- Generate a random node of given type.
; -- cog-get-all-subtypes -- Call recursively cog-get-subtypes
;
;;; Code:
; Copyright (c) 2008, 2013, 2014 Linas Vepstas <linasvepstas@gmail.com>
;

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))  ; Needed for define*-public
(use-modules (ice-9 threads))  ; Needed for par-map par-for-each

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

; A more aggressive way of doing the above:
; (define car (let ((oldcar car)) (lambda (x) (if (cog-atom? x) (oldcar (cog-outgoing-set x)) (oldcar x)))))
; But this would probably lead to various painful debugging situations.

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
(define-public (clear)
"
  clear -- extract all atoms in the atomspace. Deprecated; use
      cog-atomspace-clear instead.
"
	(cog-atomspace-clear)
)

; -----------------------------------------------------------------------
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

; ---------------------------------------------------------------------

(define-public (min-element-by-key lyst fun)
"
 min-element-by-key LIST FUN
    Given LIST and function FUN (from element to number), return the
    element of LIST for which FUN(e) is the least. That is, implements
    the argmin function.
"
	;;; No not this.
	;;; (fold (lambda (x y) (if (< x y) x y)) 1e300 (map fun lyst)))
	(fold (lambda (x y)
		(if (eq? '() x) y
		(if (eq? '() y) x
		(if (< (fun x) (fun y)) x y)))) '() lyst))

; ---------------------------------------------------------------------

(define-public (max-element-by-key lyst fun)
"
 max-element-by-key LIST FUN
    Given LIST and function FUN (from element to number), return
    the element of LIST for which FUN(e) is the highest.  That is,
    implements the argmax function.
"
	;;; No not this.
	;;; (fold (lambda (x y) (if (> x y) x y)) -1e300 (map fun lyst)))
	(fold (lambda (x y)
		(if (eq? '() x) y
		(if (eq? '() y) x
		(if (> (fun x) (fun y)) x y)))) '() lyst))

; ---------------------------------------------------------------------

(define-public cog-atomspace-stack (make-fluid '()))
(define-public cog-atomspace-stack-top (make-fluid #f))
(define-public (cog-push-atomspace)
"
 cog-push-atomspace -- Create a temporary AtomSpace.

    This creates a new AtomSpace, derived from the current AtomSpace,
    and makes it current.  Thus, all subsequent atom operations will
    create Atoms in this new AtomSpace. To delete it, simply pop it;
    after popping, all of the Atoms placed into it will also be
    deleted (unless they are referred to in some way).

    The stack of AtomSpaces is per-thread; a push in one thread does
    not affect the current AtomSpace in other threads.  There is only
    one stack per thread; changing the current AtomSpace after a push
    does not alter the stack.
"
	(define base-as (fluid-ref cog-atomspace-stack-top))
	(if (not base-as) (set! base-as (cog-atomspace)))

	(fluid-set! cog-atomspace-stack
		(cons base-as (fluid-ref cog-atomspace-stack)))

	; This assumes that cog-new-atomspace will assign a new,
	; unique name to each created AtomSpace. If this doesn't
	; happen, then multiple users might end up sharing it!
	(cog-set-atomspace! (cog-new-atomspace base-as))
	(fluid-set! cog-atomspace-stack-top (cog-atomspace))

	; Return the original (old) atomspace.
	base-as
)

; ---------------------------------------------------------------------

(define-public (cog-pop-atomspace)
"
 cog-pop-atomspace -- Delete a temporary AtomSpace.

    See cog-push-atomspace for an explanation.
"
	(let* ((stk-top (fluid-ref cog-atomspace-stack-top))
			 (stk-nxt (fluid-ref cog-atomspace-stack)))
		(if (null-list? stk-nxt)
			(throw 'badpop "More pops than pushes!"))

		; User might have done intervening cog-set-atomspace! to
		; who-knows-where, so go back to the stack to now.
		(cog-set-atomspace! stk-top)

		; Clear temp space contents. Not strictly needed,
		; but provides easier debug.
		(cog-atomspace-clear)
		(cog-set-atomspace! (car stk-nxt))

		; The push placed the temp space in the parent. Remove it
		; now, as otherwise the pointer to it will sit there,
		; more or less forever, creating an effective memleak.
		(cog-extract! stk-top)

		(fluid-set! cog-atomspace-stack-top (car stk-nxt))
		(fluid-set! cog-atomspace-stack (cdr stk-nxt))

		; Null out remaining pointers that point to the temp space
		; and force garbage collection. Without this, the temp spaces
		; hang around indefinitely. During rapid push-pop a dozen GB
		; of junk will easily accumulate. The ThreadCountUTest in the
		; atomspace-rocks unit test suite will expose this.
		(set-car! stk-nxt '())
		(set-cdr! stk-nxt '())
		(gc))

	*unspecified*
)

; ---------------------------------------------------------------------

(define rand-state-fluid (make-fluid))
(define-public (random-string str-length)
"
  random-string -- Returns a random string of length 'str-length'.

  This is now thread-safe.  I think. Its missing a unit test,
  and tends to not actually be random when hit hard from multiple
  threads. Ick.  This and everything that touches this needs
  review/redesign.
"
	(define alphanumeric "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
	(define alphlen (string-length alphanumeric))
	(define str (format #f "~A-" (get-internal-real-time)))

	; Attempt to make this thread-safe by giving each thread it's own
	; random state.
	(if (not (fluid-ref rand-state-fluid))
		(fluid-set! rand-state-fluid
			(seed->random-state (get-internal-real-time))))

	; XXX FIXME -- this is a stunningly slow and sloppy random-string
	; generator. But whatever.  I don't have the hours in the day to fix
	; everything.
	(while (> str-length 0)
		(set! str (string-append str (string (string-ref alphanumeric
			(random alphlen (fluid-ref rand-state-fluid))))))
		(set! str-length (- str-length 1))
	)
	str
)

; ---------------------------------------------------------------------

(define-public (random-node-name node-type random-length prefix)
"
  random-node-name TYPE LENGTH PREFIX - create a unique node.

  Create a random string, consisting of `PREFIX` followed by
  a random string of length `LENGTH`.  The string is checked, so
  that no node of type `TYPE` exists in the atomspace at the time
  of this call. Thus, the name is almost unique -- there still is
  a tiny window in which a race can occur.
"
	(define (check-name? node-name node-type)
	"
	  Return #t if there is a node of type node-type with name
      node-name in the current atomspace.
	"
		(not (null? (cog-node node-type node-name))))

	(define node-name (random-string random-length))
	(define prefix-length (string-length prefix))
	(if (> prefix-length 0)
		(set! node-name (string-append prefix node-name))
	)
	(while (check-name? node-name node-type)
		(if (> prefix-length 0)
			(set! node-name (string-append prefix (random-string random-length)))
			(set! node-name (random-string random-length))
		)
	)
	node-name
)

(define-public (uniquely-named-variable)
"
 uniquely-named-variable -- Creates a new uniquely-named VariableNode.
"
    (Variable (random-node-name 'VariableNode 24 "$"))
)

; -----------------------------------------------------------------------

(define-public (random-node node-type random-length prefix)
"
  Creates a random node of type `node-type`, with name `prefix` followed by
  a random string of length `random-length`. It Makes sure the resulting node
  did not previously exist in the current atomspace.

  For instance:

  (random-node 'ConceptNode 8 \"texts-\")

  returns

  (ConceptNode \"texts-218951126396-as737yFW\")
"
	(cog-new-node node-type (random-node-name node-type random-length prefix)))

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
