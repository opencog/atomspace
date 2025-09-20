;
; utilities.scm
;
;;; Commentary:
;
; Miscellaneous handy utilities for working with atoms.
; The most useful utilities in here are probably 'cog-chase-link'
; for finding atoms connected by a given link type
;
; Utilities include:
; -- simple traversal of outgoing set (gar, gdr, etc.)
; -- for-each-except loop.
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
; -- cog-get-all-nodes -- Get all the nodes within a link and its sublinks
; -- cog-filter -- filter a list of atoms, keeping the given type.
; -- cog-chase-link -- Return other atom of a link connecting two atoms.
; -- cog-map-chase-link -- Invoke proc on atoms connected through type.
; -- filter-hypergraph -- recursively traverse outgoing links of graph.
; -- cartesian-prod -- create Cartesian product from tuple of sets.
; -- cartesian-prod-list-only -- Alternative version of cartesian-prod.
; -- max-element-by-key -- Get maximum element in a list
; -- min-element-by-key -- Get maximum element in a list
; -- cog-push-atomspace -- Create a temporary atomspace.
; -- cog-pop-atomspace -- Delete a temporary atomspace.
; -- random-string -- Generate a random string of given length.
; -- random-node-name  -- Generate a random name for a node of given type.
; -- choose-var-name -- Generate a random variable name.
; -- random-node  -- Generate a random node of given type.
; -- random-variable -- Generate a random variable.
; -- cog-cp -- Copy list of atoms from one atomspace to another
; -- cog-cp-all -- Copy all atoms from one atomspace to another
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

; -----------------------------------------------------------------------
; for-each-except
(define-public (for-each-except exclude proc lst)
"
  Standard for-each loop, except that anything matching 'except' is skipped
"
	(define (loop items)
		(cond
			((null? items) #f)
			((eq? exclude (car items))
				(loop (cdr items))
			)
			(else
				(proc (car items))
				(loop (cdr items))
			)
		)
	)
	(loop lst)
)

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

  The opposite of this function is `cog-get-all-nodes`, which
  returns the leaves under ATOM.

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
(define-public (cog-get-all-nodes LINK)
"
  cog-get-all-nodes ATOM -- Get all the nodes (leaves) under `ATOM`.

  Get all the nodes (leaves) within a hypergraph, and return as
  a list.

  See also: cog-get-root, cog-get-trunk
"
	(define (recursive-helper ATOM)
		(if (cog-link? ATOM)
			(cog-get-all-nodes ATOM)
			(list ATOM)
		)
	)

	(if (cog-node? LINK)
		(list LINK)
		(append-map recursive-helper (cog-outgoing-set LINK)))
)

; -----------------------------------------------------------------------
(define-public (cog-filter ATOM-TYPE ATOM-LIST)
"
  cog-filter ATOM-TYPE ATOM-LIST -- filter the scheme list
  ATOM-LIST, keeping only the atoms of ATOM-TYPE.
"
	(define (is-type? atom) (eq? ATOM-TYPE (cog-type atom)))
	(filter is-type? ATOM-LIST)
)

; -----------------------------------------------------------------------
(define-public (cog-chase-link link-type endpoint-type anchor)
"
  cog-chase-link -- Return other atom of a link connecting two atoms.

  cog-chase-link link-type endpoint-type anchor

  Starting at the atom 'anchor', chase its incoming links of
  'link-type', and return a list of all of the atoms of type
  'endpoint-type' in those links. For example, if 'anchor' is the
  node 'GivenNode \"a\"', and the atomspace contains

     SomeLink
         GivenNode \"a\"
         WantedNode  \"p\"

     SomeLink
         GivenNode \"a\"
         WantedNode  \"q\"

  then this method will return the two WantedNodes's, given the
  GivenNode as anchor, and the link-type 'SomeLink.

  viz: (cog-chase-link 'SomeLink 'WantedNode (GivenNode \"a\")) will
  return ((WantedNode \"p\") (WantedNode \"q\"))

  It is presumed that 'anchor' points to some atom (typically a node),
  and that it has many links in its incoming set. So, loop over all of
  the links of 'link-type' in this set. They presumably link to all
  sorts of things. Find all of the things that are of 'endpoint-type'.
  Return a list of all of these.

  See also: cgw-follow-link, which does the same thing, but for wires.
"
	(let ((lst '()))
		(define (mklist inst)
			(set! lst (cons inst lst))
			#f
		)
		(cog-map-chase-link link-type endpoint-type mklist anchor)
		lst
	)
)

; -----------------------------------------------------------------------
(define-public (cog-map-chase-link link-type endpoint-type proc anchor)
"
  cog-map-chase-link -- Invoke proc on atom connected through type.

  Similar to cog-chase-link, but invokes 'proc' on the wanted atom.
  Starting at the atom 'anchor', chase its incoming links of
  'link-type', and call procedure 'proc' on all of the atoms of
  type 'endpoint-type' in those links. For example, if 'anchor' is the
  node 'GivenNode \"a\"', and the atomspace contains

     SomeLink
         GivenNode \"a\"
         WantedNode  \"p\"

     SomeLink
         GivenNode \"a\"
         WantedNode  \"q\"

  then 'proc' will be called twice, with each of the WantedNodes's
  as the argument. These wanted nodes were found by following the
  link type 'SomeLink, starting at the anchor GivenNode \"a\".

  It is presumed that 'anchor' points to some atom (typically a node),
  and that it has many links in its incoming set. So, loop over all of
  the links of 'link-type' in this set. They presumably link to all
  sorts of things. Find all of the things that are of 'endpoint-type'.
  Apply proc to each of these.
"
	(define (get-endpoint w)
		(map proc (cog-outgoing-by-type w endpoint-type))
	)

	; We assume that anchor is a single atom, or empty list...
	(if (null? anchor)
		'()
		(map get-endpoint (cog-incoming-by-type anchor link-type))
	)
)

; ---------------------------------------------------------------------
(define-public (filter-hypergraph pred? atom-list)
"
  filter-hypergraph -- recursively traverse outgoing links of graph.

  filter-hypergraph pred? atom-list

  Given a list of atoms, and a scheme-predicate pred?, return a
  list of atoms that satisfy the scheme-predicate.  This is not
  a simple srfi-1 'filter', rather, it traverses the hypergraph,
  applying the predicate to the subgraphs.

  In the current implementation, the scheme-predicate is assumed to
  select only for Nodes.
"
	(define (fv atoms lst)
		(cond
			; If its a query word, append it to the list
			((cog-node? atoms)
				(if (pred? atoms)
					(cons atoms lst)
					lst
				)
			)

			; If its a link, scan its outgoing set
			((cog-link? atoms)
				(fv (cog-outgoing-set atoms) lst)
			)

			; If its a list then scan the list
			((pair? atoms)
				(append!
					(append-map!
						(lambda (x) (filter-hypergraph pred? x)) atoms)
					lst
				)
			)
		)
	)
	(fv atom-list '())
)

; --------------------------------------------------------------------
(define-public (cartesian-prod tuple-of-lists)
"
  cartesian-prod -- create Cartesian product from tuple of sets.

  This returns the Cartestion product of a tuple of sets by distributing
  the product across the set elements. Returned is a list of tuples, where
  the elements of the tuple are elements from the corresponding sets.

  Let the input tuple be [s_1, s_2, ..., s_m] where each s_k is a set of
  one or more elements.  Let the cardinality of s_k be n_k.
  Then this returns a list of n_1 * n_2 *... * n_m tuples.
  where the projection of the coordinate k is an element of s_k.
  That is, let p_k be the k'th coordinate projection, so that
  p_k [a_1, a_2, ... , a_m] = a_k

  Then, if t is a tuple returned by this function, one has that
  p_k(t) is element of s_k for all t and all 1<= k <= m. Every possible
  combination of set elements is returned.

  Example usage and output:

  guile> (cartesian-prod (list 'p 'q))
  (p q)
  guile> (cartesian-prod (list 'p (list 'x 'y)))
  ((p x) (p y))
  guile> (cartesian-prod (list (list 'p 'q) 'x))
  ((p x) (q x))
  guile> (cartesian-prod (list (list 'p 'q) (list 'x 'y)))
  ((p x) (q x) (p y) (q y))
  guile> (cartesian-prod (list (list 'p 'q) 'a (list 'x 'y)))
  ((p a x) (q a x) (p a y) (q a y))
  guile> (cartesian-prod (list 'a (list 'p 'q) (list 'x 'y)))
  ((a p x) (a q x) (a p y) (a q y))

  XXX TODO -- this would almost surely be simpler to implement using
  srfi-1 fold or srfi-1 map, which is really all that it is ...
"

	; distribute example usage:
	; guile> (distribute (list 'p 'q)  (list '1 '2 '3))
	; ((p 1 2 3) (q 1 2 3))
	; guile> (distribute 'p (list '1 '2 '3))
	; (p 1 2 3)
	; guile> (distribute 'p '1)
	; (p 1)
	; guile> (distribute (list 'p 'q) '1)
	; ((p 1) (q 1))
	;
	(define (distribute heads a-tail)
		(define (match-up a-head tale)
			(if (list? tale)
				(cons a-head tale)
				(list a-head tale)
			)
		)
		(if (list? heads)
			(map (lambda (hed) (match-up hed a-tail)) heads)
			(match-up heads a-tail)
		)
	)

	; guile> (all-combos 'p (list 'x 'y))
	; ((p x) (p y))
	; guile> (all-combos 'p (list (list 'a 'b) (list 'y 'z)))
	; ((p a b) (p y z))
	; guile> (all-combos (list 'p 'q) (list 'x 'y))
	; ((p x) (q x) (p y) (q y))
	; guile> (all-combos (list 'p 'q) 'x)
	; ((p x) (q x))
	; guile> (all-combos 'p  'x)
	; (p x)
	;
	(define (all-combos heads tails)
		(if (list? tails)
			(let ((ll (map (lambda (tale) (distribute heads tale)) tails)))
				(if (list? heads)
					(concatenate! ll)
					ll
				)
			)

			; tails is not a list
			(distribute heads tails)
		)
	)

	(cond
		((null? tuple-of-lists) '())
		((not (list? tuple-of-lists)) tuple-of-lists)
		(else
			(let ((lc (car tuple-of-lists))
					(rc (cdr tuple-of-lists))
				)
				(if (null? rc)
					lc
					; cartesian-prod always returns tails, so we
					; just need to take all combinations of the
					; head against the returned tails.
					; (This is not a tail-recursive call but I don't care)
					(all-combos lc (cartesian-prod rc))
				)
			)
		)
	)
)

; ---------------------------------------------------------------------
(define-public (cartesian-prod-list-only tuple-of-lists)
"
  cartesian-prod-list-only -- Alternative version of cartesian-prod.

  Similar to cartesian-prod, but everything in 'tuple-of-lists' must be
  a list.  ie. cannot do (cartesian-prod-list-only (list 'p 'q))

  The different in this version is that given the input '(((1) (2)) ((3)))
  the above version returns (((1) 3)) ((2) 3)), but this version returns
  (((1) (3)) ((2) (3))).
"
	(fold-right
		; lst1: one of the list in the tuple
		; lst2: cartesian product of the rest
        	(lambda (lst1 lst2)
        		; choose each item in lst1 as 'head', and add it to the cartesian product of lst2
			(append-map
				(lambda (head)
					(if (null? lst2)
						(list (cons head '()))
						(map (lambda (rest-in-cart) (cons head rest-in-cart)) lst2)
					)
				)
				lst1
			)
        	)
		'()
		tuple-of-lists
	)
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

		; Guile gc should eventually garbage-collect this atomspace,
		; which will clear it. But ... even when brute-forcing the
		; gc to run (as done below), the atomspace seems to hang
		; around anyway, undeleted. So we brute-force clear it now,
		; so that at least the atoms do not chew up RAM.
		(cog-atomspace-clear)
		(cog-set-atomspace! (car stk-nxt))
		(fluid-set! cog-atomspace-stack-top (car stk-nxt))
		(fluid-set! cog-atomspace-stack (cdr stk-nxt))

		; Try to force garbage-collection of the atomspace.
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

(define-public (choose-var-name)
"
 DEPRECATED - use uniquely-named-variable instead.
"
    (random-node-name 'VariableNode 24 "$")
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

(define-public (random-variable)
"
 Creates a new random VariableNode.
"
    (random-node 'VariableNode 36 "$")
)

; -----------------------------------------------------------------------

(define-public (cog-cp ATOMSPACE ATOM-LIST)
"
  cog-cp ATOMSPACE ATOM-LIST - Copy the atoms in ATOM-LIST to ATOMSPACE.
  Returns the list of copied atoms.
"
	(map (lambda (AT) (cog-new-atom AT ATOMSPACE)) ATOM-LIST)
)

; -----------------------------------------------------------------------
(define-public (cog-cp-all ATOMSPACE)
"
  cog-cp-all AS - Copy all atoms in the current atomspace for this
     thread to ATOMSPACE.  Return the list of copied atoms.
"
  (cog-cp ATOMSPACE (cog-get-all-roots))
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

(define-public (cog-mean ATOM)
"
 cog-mean ATOM
    Return the `mean` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-confidence, cog-count, cog-tv
"
	(cog-tv-mean (cog-tv ATOM)))

(define-public (cog-confidence ATOM)
"
 cog-confidence ATOM
    Return the `confidence` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-count, cog-tv
"
	(cog-tv-confidence (cog-tv ATOM)))

(define-public (cog-count ATOM)
"
 cog-count ATOM
    Return the `count` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-confidence, cog-tv, cog-inc-count!
"
	(cog-tv-count (cog-tv ATOM)))

; ---------------------------------------------------------------------
