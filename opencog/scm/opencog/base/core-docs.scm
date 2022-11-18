;
; core-docs.scm
;
; Provide documentation for all the functions implemented in C++ code.
; These can be viewed the guile interpreter prompt by saying
;
;    guile> ,describe  FUNCTION-NAME
;
; A list of all of these is printed by saying
;
;    guile> ,apropos cog
;
(set-procedure-property! cog-new-atom 'documentation
"
 cog-new-atom ATOM [ATOMSPACE]
    If the optional ATOMSPACE argument is provided, copy the existing
    ATOM into ATOMSPACE; otherwise copy it into the current AtomSpace
    for this thread.

    Use (cog-atomspace ATOM) to examine the AtomSpace that the ATOM
    is currently in.

    Example:
        ; Create a new Atom in the current AtomSpace.
        guile> (define ca (Concept \"A\"))

        ; What AtomSpace is it in?
        guile> (cog-atomspace ca)

        ; Create a new AtomSpace, and put an atom into it.
        guile> (define spacex (cog-new-atomspace))
        guile> (define xca (cog-new-atom ca spacex))
        guile> (cog-atomspace xca)

        ; Change the TV on it, just so that it is easier to spot.
        guile> (cog-set-tv! xca (SimpleTruthValue 0.2 0.2))

        ; Print AtomSpace contents
        guile> (cog-prt-atomspace)
        guile> (cog-prt-atomspace spacex)
")

(set-procedure-property! cog-new-node 'documentation
"
 cog-new-node NODE-TYPE NODE-NAME [ATOMSPACE] [TV]
    Create a new Node of Type NODE-TYPE and name NODE-NAME.
    Optionally, place it in ATOMSPACE and/or assign a TruthValue TV
    to it.

    Throws errors if NODE-TYPE is not a valid Atom Type for a Node,
    and if NODE-NAME is not a string. Use (cog-get-types) to get a
    list of the currently loaded atom types.

    Example:
        ; Create a new node, and prints its value:
        guile> (cog-new-node 'Concept \"some node name\")
        (ConceptNode \"some node name\")

        ; Creates a new node, with a truth value:
        guile> (cog-new-node 'Concept \"another node\"
                      (SimpleTruthValue 0.8 0.9))
        (ConceptNode \"another node\" (SimpleTruthValue 0.8 0.9))

        ; Creates a new atomspace, and places the node there:
        guile> (define spacex (cog-new-atomspace))
        guile> (Concept \"foo\" spacex)
        guile> (cog-prt-atomspace spacex)
")

(set-procedure-property! cog-node 'documentation
"
 cog-node NODE-TYPE NODE-NAME [ATOMSPACE] [NEW-TV]
    Returns the Node of Type NODE-TYPE and name NODE-NAME, if it exists,
    else returns null.

    If an optional ATOMSPACE is specified, then it is queried for the
    node, instead of the current AtomSpace for this thread. If an
    optional TruthValue NEW-TV is specified, and if the atom exists,
    then the TruthValue is changed to NEW-TV.

    Throws errors if NODE-TYPE is not a valid atom type for a Node,
    and if NODE-NAME is not a string. Use (cog-get-types) to get a
    list of the currently loaded atom types.

    Example:
        ; Check to see if a node exists:
        guile> (cog-node 'Concept \"asdf\")
        ()

        ; Verify that the return value is actually a true null:
        guile> (null? (cog-node 'Concept \"asdf\"))
        #t

        ; Now, create the node, and see if it exists:
        guile> (cog-new-node 'Concept \"asdf\")
        (ConceptNode \"asdf\")
        guile> (null? (cog-node 'Concept \"asdf\"))
        #f

        ; Change the truth value of an existing node:
        guile> (cog-node 'Concept \"asdf\" (SimpleTruthValue 0.8 0.9))
        (ConceptNode \"asdf\" (stv 0.8 0.9))
")

(set-procedure-property! cog-new-link 'documentation
"
 cog-new-link LINK-TYPE ATOM-1 ... ATOM-N [ATOMSPACE] [TV]
    Create a new Link, of Type LINK-TYPE, holding the given Atoms.
    Optionally, place it in ATOMSPACE and/or assign a TruthValue TV
    to it.

    Throws errors if LINK-TYPE is not a valid Link Type, or if any of
    the arguments after the Link Type are not Atoms, TruthValues or
    AtomSpaces. Use (cog-get-types) to get a list of the currently
    loaded atom types.

    Example:
        ; Creates two nodes, and a new link:
        guile> (define x (Concept \"abc\"))
        guile> (define y (Concept \"def\"))
        guile> (cog-new-link 'Link x y)
        (Link
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )

        ; Create a new link with a truth value:
        guile> (cog-new-link 'Link x y (SimpleTruthValue 0.7 0.8))
        (Link (stv 0.7 0.8)
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )

        ; Creates a new atomspace, and places the link there:
        guile> (define spacex (cog-new-atomspace))
        guile> (cog-new-link 'Link x y spacex)
        guile> (cog-prt-atomspace spacex)
")

(set-procedure-property! cog-link 'documentation
"
 cog-link LINK-TYPE ATOM-1 ... ATOM-N [ATOMSPACE] [NEW-TV]
    Returns the Link of the given type LINK-TYPE and list of atoms,
    if it exists, else returns null.

    If an optional ATOMSPACE is specified, then it is queried for the
    link, instead of the current AtomSpace for this thread. If an
    optional TruthValue NEW-TV is specified, and if the atom exists,
    then the TruthValue is changed to NEW-TV.

    Throws errors if LINK-TYPE is not a valid Link Type, or if any
    of the arguments after the Link Type are not Atoms, TruthValues or
    AtomSpaces. Use (cog-get-types) to get a list of the currently
    loaded atom types.

    Example:
        ; Create two nodes:
        guile> (define x (cog-new-node 'ConceptNode \"abc\"))
        guile> (define y (cog-new-node 'ConceptNode \"def\"))

        ; Does a node with these two links exist?
        guile> (cog-link 'Link x y)
        ()

        ; Now, create such a link
        guile> (cog-new-link 'Link x y)
        (Link
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )

        ; Check again for existence:
        guile> (cog-link 'Link x y)
        (Link
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )

        ; Change the truth value of an existing node:
        guile> (cog-link 'Link x y (SimpleTruthValue 0.7 0.8))
        (Link (stv 0.7 0.8)
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )
")

(set-procedure-property! cog-extract! 'documentation
"
 cog-extract! ATOM [ATOMSPACE]
    Remove the indicated ATOM, but only if it has no incoming links.
    If it has incoming links, the remove fails.

    Returns #t if the atom was removed, else returns #f if not removed.

    This does NOT remove the atom from any attached persistent storage.
    Use cog-delete! from the (opencog persist) module to remove atoms
    from storage.

    Use cog-extract-recursive! to force removal of this atom, together
    with any links that might be holding this atom.

    If the optional ATOMSPACE argument is provided, then the ATOM is
    removed from that AtomSpace; otherwise, it is removed from the
    current AtomSpace for this thread.

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (Concept \"abc\"))
       guile> (define y (Concept \"def\"))
       guile> (define l (Link x y))

       ; Verify that there's an atom called x:
       guile> x
       (ConceptNode \"abc\")

       ; Try to extract x. This should fail, since there's a link
       ; containing x.
       guile> (cog-extract! x)
       #f

       ; Delete x, and everything pointing to it. This should extract
       ; both x, and the link l.
       guile> (cog-extract-recursive! x)
       #t

       ; Verify that the link l is gone:
       guile> l
       Invalid handle

       ; Verify that the node x is gone:
       guile> x
       Invalid handle

       ; Verify that the node y still exists:
       guile> y
       (ConceptNode \"def\")
")

(set-procedure-property! cog-extract-recursive! 'documentation
"
 cog-extract-recursive! ATOM [ATOMSPACE]
    Remove the indicated ATOM, and all atoms that point at it.
    Return #t on success, else return #f if not removed.

    This does NOT remove the atom from any attached persistent storage.
    Use cog-delete-recursive! from the (opencog persist) module to
    remove atoms from storage.

    If the optional ATOMSPACE argument is provided, then the ATOM is
    removed from that AtomSpace; otherwise, it is removed from the
    current AtomSpace for this thread.

    See also: cog-extract!
")

(set-procedure-property! cog-atom? 'documentation
"
 cog-atom? EXP
    Return #t if EXP is an atom, else return #f

    Example:
       ; Define a node
       guile> (define x (Concept \"abc\"))
       guile> (define y (+ 2 2))
       guile> (cog-atom? x)
       #t
       guile> (cog-atom? y)
       #f
")

(set-procedure-property! cog-node? 'documentation
"
 cog-node? EXP
    Return #t if EXP is an node, else return #f

    See also cog-node, which will check to see if a specific node
    already exists.

    Example:
       ; Define a node and a link
       guile> (define x (Concept \"abc\"))
       guile> (define y (ListLink x))
       guile> (cog-node? x)
       #t
       guile> (cog-node? y)
       #f
")

(set-procedure-property! cog-link? 'documentation
"
 cog-link? EXP
    Return #t if EXP is an link, else return #f

    See also cog-link, which will check to see if a specific link
    already exists.

    Example:
       ; Define a node and a link
       guile> (define x (Concept \"abc\"))
       guile> (define y (ListLink x))
       guile> (cog-link? x)
       #f
       guile> (cog-link? y)
       #t
")

(set-procedure-property! cog-name 'documentation
"
 cog-name ATOM
    Return the name of the node ATOM. If the atom is not a node,
    returns null.

    Example:
       ; Define a node
       guile> (define x (Concept \"abc\"))
       guile> (cog-name x)
       \"abc\"
")

(set-procedure-property! cog-number 'documentation
"
 cog-number NUMBER-NODE
    Return the (list of) floating point values of NUMBER-NODE.
    The NUMBER-NODE must be an Node of type NumberNode, else an
    exception will be thrown.

   This is the same as saying (string->number (cog-name NUMBER-NODE))

    Example:
       ; Define a node
       guile> (define x (cog-new-node 'NumberNode 42))
       guile> (cog-number x)
       42.0
")

(set-procedure-property! cog-type 'documentation
"
 cog-type EXP
    Return the type of EXP, where EXP is a Value or an Atom.
    The returned value is a guile symbol.

    Example:
       ; Define a node
       guile> (define x (Concept \"abc\"))
       guile> (cog-type x)
       ConceptNode
       guile> (eq? 'ConceptNode (cog-type x))
       #t

    See also:
        cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
")

(set-procedure-property! cog-arity 'documentation
"
 cog-arity ATOM
    Return the arity of ATOM.

    Example:
       guile> (define x (Concept \"abc\"))
       guile> (cog-arity x)
       0
       guile> (define l (Link x x x))
       guile> (cog-arity l)
       3
")

(set-procedure-property! cog-incoming-set 'documentation
"
 cog-incoming-set ATOM [ATOMSPACE]
    Return the incoming set of ATOM.  This set is returned as an
    ordinary scheme list.

    If the optional argument ATOMSPACE is given, then the lookup is
    performed in that AtomSpace. This is useful when the Atom appears
    in more than one AtomSpace, and it's incoming set differs in each
    space.  If the optional argument is not given, then the current
    AtomSpace is used.

    See also: cog-incoming-size, cog-incoming-by-type

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (Concept \"abc\"))
       guile> (define y (Concept \"def\"))
       guile> (define l (Link x y))

       ; Get the incoming sets of nodes x and y (which is the link l):
       guile> (cog-incoming-set x)
       ((Link
          (ConceptNode \"abc\")
          (ConceptNode \"def\")
       )
       )

       guile> (cog-incoming-set y)
       ((Link
          (ConceptNode \"abc\")
          (ConceptNode \"def\")
       )
       )

       ; Verify that the both incoming sets are really one and the
       ; same link:
       guile> (equal? (cog-incoming-set x) (cog-incoming-set y))
       #t

       ; The returned values are lists, and not singleton atoms.
       ; Thus, the incoming set of x is a list containing l:
       guile> (equal? (cog-incoming-set x) (list l))
       #t

       ; Verify that the returned value is a true list:
       guile> (list? (cog-incoming-set x))
       #t
")

(set-procedure-property! cog-incoming-size 'documentation
"
 cog-incoming-size ATOM [ATOMSPACE]
    Return the number of atoms in the incoming set of ATOM.

    If the optional argument ATOMSPACE is given, then the lookup is
    performed in that AtomSpace. This is useful when the Atom appears
    in more than one AtomSpace, and it's incoming set differs in each
    space.  If the optional argument is not given, then the current
    AtomSpace is used.

    See also: cog-incoming-set, cog-incoming-size-by-type

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (Concept \"abc\"))
       guile> (define y (Concept \"def\"))
       guile> (Link x y)

       ; Get the size of the incoming set of nodes x and y:
       guile> (cog-incoming-size x)
       => 1

       guile> (cog-incoming-size y)
       => 1
")

(set-procedure-property! cog-incoming-by-type 'documentation
"
 cog-incoming-by-type ATOM TYPE [ATOMSPACE]
    Return the incoming set of ATOM that consists only of atoms of
    type TYPE.  This set is returned as an ordinary scheme list.

    Equivalent to (cog-filter TYPE (cog-incoming-set ATOM)), but
    should be faster, performance-wise.

    If the optional argument ATOMSPACE is given, then the lookup is
    performed in that AtomSpace. This is useful when the Atom appears
    in more than one AtomSpace, and it's incoming set differs in each
    space.  If the optional argument is not given, then the current
    AtomSpace is used.

    Example:
       ; Define two nodes and two links between them:
       guile> (define x (Concept \"abc\"))
       guile> (define y (Concept \"def\"))
       guile> (ListLink x y)
       guile> (UnorderedLink x y)

       ; Get all ListLinks that x appears in:
       guile> (cog-incoming-by-type x 'ListLink)
       ((ListLink
          (ConceptNode \"abc\")
          (ConceptNode \"def\")
       )
       )

       ; Get all UnorderedLinks that x appears in:
       guile> (cog-incoming-by-type x 'UnorderedLink)
       ((UnorderedLink
          (ConceptNode \"abc\")
          (ConceptNode \"def\")
       )
       )
")

(set-procedure-property! cog-incoming-size-by-type 'documentation
"
 cog-incoming-size-by-type ATOM TYPE [ATOMSPACE]
    Return the number of atoms of type TYPE in the incoming set of ATOM.

    If the optional argument ATOMSPACE is given, then the lookup is
    performed in that AtomSpace. This is useful when the Atom appears
    in more than one AtomSpace, and it's incoming set differs in each
    space.  If the optional argument is not given, then the current
    AtomSpace is used.

    See also: cog-incoming-by-type, cog-incoming-size

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (Concept \"abc\"))
       guile> (define y (Concept \"def\"))
       guile> (ListLink x y)
       guile> (UnorderedLink x y)

       ; Get the number of ListLinks that x appears in:
       guile> (cog-incoming-size-by-type x 'ListLink)
       => 1
")

(set-procedure-property! cog-outgoing-atom 'documentation
"
 cog-outgoing-atom ATOM INDEX
    Return the INDEX'th atom in the outgoing set of ATOM. Indexing
    is done from a base of zero. This returns the same atom as
    (list-ref (cog-outgoing-set ATOM) INDEX) but is faster.
")

(set-procedure-property! cog-outgoing-set 'documentation
"
 cog-outgoing-set ATOM
    Return the outgoing set of ATOM.  This set is returned as an
    ordinary scheme list.
")

(set-procedure-property! cog-outgoing-by-type 'documentation
"
 cog-outgoing-by-type ATOM TYPE
    Return those atoms in the outgoing set of ATOM that are of type TYPE.
    This set is returned as an ordinary scheme list.

    Equivalent to (cog-filter TYPE (cog-outgoing-set ATOM)), but
    should be faster, performance-wise.
")

(set-procedure-property! cog-handle 'documentation
"
 cog-handle ATOM
    Return the hash of ATOM. The hash is a 64-bit integer, computed
    from the component parts of the Atom (but not it's Values), that
    can be used in hash tables or other algorithms that require a hash.
    Links always have the high-bit set, and Nodes never do.

    Example:
       guile> (cog-handle (Concept \"abc\"))
       999283543311182409
")

(set-procedure-property! cog-atom-less? 'documentation
"
 cog-atom-less? L-ATOM R-ATOM
    Return #t if L-ATOM is less than R-ATOM, else return #f.  This
    predicate is useful for creating sorted lists of atoms; for
    example, to rapidly remove duplicate atoms from a long list.

    Example:
       guile> (cog-atom-less? (Concept \"abc\") (Concept \"def\"))
       #f
")

(set-procedure-property! cog-equal? 'documentation
"
 cog-equal? L-ATOM R-ATOM
    Return #t if the contents of L-ATOM and R-ATOM are the same, else
    return #f.  This can be used to compare two atoms in two different
    AtomSpaces for equality. A content-compare is performed, i.e. a
    check is made to see if they have the same name or the same outgoing
    set. The Values hanging off the atoms are NOT compared; they might
    differ.

    Example:
       guile> (define space1 (cog-atomspace))
       guile> (define a1 (Concept \"abc\"))
       guile> (define space2 (cog-new-atomspace))
       guile> (cog-set-atomspace! space2)
       guile> (define a2 (Concept \"abc\"))
       guile> (equal? a1 a2)
       #f
       guile> (cog-equal? a1 a2)
       #t
")

(set-procedure-property! cog-inc-count! 'documentation
"
  cog-inc-count! ATOM CNT -- Increment count truth value on ATOM by CNT.

  Atomically increment the count on a CountTruthValue by CNT. The mean
  and confidence values are left untouched.  CNT may be any floating
  point number (positive or negative).

  If the current truth value on the ATOM is not a CountTruthValue,
  then the truth value is replaced by a CountTruthValue, with the
  count set to CNT.

  The increment is atomic; that is, it is safe against racing threads.

  Example usage:
     (cog-inc-count! (Concept \"Answer\") 42.0)

  See also:
      cog-count -- Fetch the current count.
      cog-inc-value! -- Increment an arbitrary FloatValue.
      cog-update-value! -- A generic atomic read-modify-write.
")

(set-procedure-property! cog-inc-value! 'documentation
"
  cog-inc-value! ATOM KEY CNT REF -- Increment value on ATOM by CNT.

  The REF location of the FloatValue at KEY is atomically incremented
  by CNT.  CNT may be any floating-point number (positive or negative).
  The rest of the FloatValue vector is left untouched.

  If the ATOM does not have any Value at KEY, or if the current Value
  is not a FloatValue, then a new FloatValue of length (REF+1) is
  created. If the existing FloatValue is too short, it is extended
  until it is at least (REF+1) in length.

  The increment is atomic; that is, it is safe against racing threads.

  To increment several locations at once, use the cog-update-value!
  function.

  Example usage:
     (cog-inc-value!
         (Concept \"Question\")
         (Predicate \"Answer\")
         42.0  0)

  See also:
      cog-inc-count! -- Increment the CountTruthValue.
      cog-update-value! -- A generic atomic read-modify-write.
      cog-set-value-ref! - Set one location in a vector.
      cog-inc-value-ref! - Increment one location in a vector.
")

(set-procedure-property! cog-mean 'documentation
"
 cog-mean ATOM
    Return the `mean` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-confidence, cog-count, cog-tv
")

(set-procedure-property! cog-confidence 'documentation
"
 cog-confidence ATOM
    Return the `confidence` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-count, cog-tv
")

(set-procedure-property! cog-count 'documentation
"
 cog-count ATOM
    Return the `count` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-confidence, cog-tv, cog-inc-count!
")

; ===================================================================

(set-procedure-property! cog-tv 'documentation
"
 cog-tv ATOM
    Return the truth-value of ATOM.

    Example:
       ; Define a node
       guile> (define x
                 (Concept \"abc\" (SimpleTruthValue 0.2 0.5)))
       guile> (cog-tv x)
       (stv 0.2 0.5)
       guile> (cog-tv? (cog-tv x))
       #t

    See also: cog-set-tv!
")

(set-procedure-property! cog-set-tv! 'documentation
"
 cog-set-tv! ATOM TV
    Set the truth-value of ATOM to TV.

    Example:
       ; Define a node
       guile> (define x (Concept \"def\"))
       guile> (cog-tv x)
       (stv 1 0)
       guile> (cog-set-tv! x (SimpleTruthValue 0.9 0.8))
       (ConceptNode \"def\" (stv 0.9 0.8))
       guile> (cog-tv x)
       (stv 0.9 0.8)
")

(set-procedure-property! cog-tv-mean 'documentation
"
 cog-tv-mean TV
    Return the `mean` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-mean
")

(set-procedure-property! cog-tv-confidence 'documentation
"
 cog-tv-confidence TV
    Return the `confidence` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-confidence
")

(set-procedure-property! cog-tv-count 'documentation
"
 cog-tv-count TV
    Return the `count` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-count
")

; ===================================================================
;

(set-procedure-property! cog-new-value 'documentation
"
 cog-new-value TYPE ARGS
    Create a new Value of type TYPE, with additional ARGS. In many
    cases, ARGS is list of strings, floats or values.  The TYPE must
    be a valid Value type; use (cog-get-types) to get a list of the
    currently loaded types.

    Example:
       guile> (cog-new-value 'FloatValue 1 2 3))
       (FloatValue 1.000000 2.000000 3.00000)

       guile> (cog-new-value 'StringValue \"foo\" \"bar\")
       (StringValue \"foo\" \"bar\")

       guile> (cog-new-value 'LinkValue
             (Concept \"foo\") (StringValue \"bar\"))
       (LinkValue
           (ConceptNode \"foo\")
           (StringValue \"bar\")
       )

       guile> (cog-new-value 'Concept \"foo\")
       (ConceptNode \"foo\")
")

(set-procedure-property! cog-keys 'documentation
"
 cog-keys ATOM
    Return a list of all of the keys attached to ATOM. In order to get
    all of the values attached to an atom, one must first find the keys
    that are used to anchor them. This function returns these.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-keys (Concept \"abc\"))

    See also:
       cog-keys->alist ATOM - return association list of keys+values
       cog-value ATOM KEY - return a value for the given KEY
       cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
")

(set-procedure-property! cog-keys->alist 'documentation
"
 cog-keys->alist ATOM
    Return an association list of all key-value pairs attached to ATOM.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-keys->alist (Concept \"abc\"))

    See also:
       cog-keys ATOM - return list of all keys on ATOM
       cog-value ATOM KEY - return a value for the given KEY
       cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
")

(set-procedure-property! cog-value 'documentation
"
 cog-value ATOM KEY
    Return the value of KEY for ATOM. Both ATOM and KEY must be atoms.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-value (Concept \"abc\") (Predicate \"key\"))
       (FloatValue 1.000000 2.000000 3.00000)

   See also:
       cog-keys ATOM - return list of all keys on ATOM
       cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
")

(set-procedure-property! cog-value-type 'documentation
"
 cog-value-type ATOM KEY
    Return the type of the value of KEY for ATOM. Both ATOM and KEY
    must be atoms. The returned type is a guile symbol.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-value-type (Concept \"abc\") (Predicate \"key\"))
       FloatValue

   See also:
       cog-value ATOM KEY -- get the value at KEY on ATOM.
       cog-keys ATOM - return list of all keys on ATOM.
")

(set-procedure-property! cog-set-value! 'documentation
"
 cog-set-value! ATOM KEY VALUE
    Set the value of KEY for ATOM to VALUE. Both ATOM and KEY must be
    atoms. The VALUE can be any Atomese Value, or #f or the empty list.
    If it is #f or the empty list '(), then the KEY is removed from
    the atom.

    This returns either ATOM or a copy of ATOM with the new value. If
    the current AtomSpace is a copy-on-write (COW) AtomSpace, and ATOM
    lies in some other space below the current space, then a copy of
    ATOM will be made and placed in the current space.  This copy will
    have the new VALUE, while the value on the input ATOM will be
    unchanged.  COW spaces are commonly used with underlying read-only
    spaces, or with long stacks (DAG's) of AtomSpaces (Frames) recording
    a history of value changes.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-value (Concept \"abc\") (Predicate \"key\"))
       (FloatValue 1.000000 2.000000 3.00000)

       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\") #f)
       guile> (cog-value (Concept \"abc\") (Predicate \"key\"))
       #f

    See also:
       cog-set-value-ref! - Set one location in a vector.
       cog-inc-value-ref! - Increment one location in a vector.
       cog-update-value! - Perform an atomic read-modify-write
       cog-set-values! - Set multiple values.
       cog-new-atomspace - Create a new AtomSpace
       cog-atomspace-cow! - Mark AtomSpace as a COW space.
       cog-atomspace-ro! - Mark AtomSpace as read-only.
")

(set-procedure-property! cog-update-value! 'documentation
"
 cog-update-value! ATOM KEY DELTA
    Perform an atomic read-modify-write update of the value at KEY
    on ATOM, using DELTA to modify the original value.

    At this time, the only updates that are implemented are the
    increment of floating-point Values, such as FloatValues and
    TruthValues. The intended use is for the safe update of counts
    from multiple threads.

    This function is similar to the `cog-inc-value!` function, except
    that it allows a full-vector update. When DELTA is a vector, with
    multiple non-zero entries, all of them are added to the matching
    entries in the Value at KEY. This is a vector-increment.

    The update is atomic; that is, it is safe against racing threads.

    See also:
       cog-inc-count! -- Increment a CountTruthValue
       cog-inc-value! -- Increment a generic FloatValue
       cog-set-value! -- Set a single value.
       cog-set-values! -- Set multiple values.
       cog-set-value-ref! - Set one location in a vector.
       cog-inc-value-ref! - Increment one location in a vector.
")

(set-procedure-property! cog-set-values! 'documentation
"
 cog-set-values! ATOM ALIST
    Set multiple values on ATOM from the key-value pairs in ALIST.
    The ALIST must be an association list, of the form of
    ((key1 . value1) (key2 . value2) ...)

    Example:
       guile> (cog-set-values!
                 (Concept \"abc\")
                 (list
                    (cons (Predicate \"key1\") (FloatValue 1 2 3))
                    (cons (Predicate \"key2\") (FloatValue 4 5 6))))
       guile> (cog-keys->alist (Concept \"abc\"))

    Keys can also be removed, by setting the value to #f or to '()

    Example:
       guile> (cog-set-values!
                 (Concept \"abc\")
                 (list
                    (cons (Predicate \"key1\") #f)
                    (cons (Predicate \"key2\") #f)))
       guile> (cog-keys->alist (Concept \"abc\"))

    This returns either ATOM or a copy of ATOM with the new value. If
    the current AtomSpace is a copy-on-write (COW) AtomSpace, and ATOM
    lies in some other space below the current space, then a copy of
    ATOM will be made and placed in the current space.  This copy will
    have the new VALUE, while the value on the input ATOM will be
    unchanged.  COW spaces are commonly used with underlying read-only
    spaces, or with long stacks (DAG's) of AtomSpaces (Frames) recording
    a history of value changes.

    See also:
       cog-set-value! ATOM VALUE - Set a single value.
       cog-new-atomspace - Create a new AtomSpace
       cog-atomspace-cow! BOOL - Mark AtomSpace as a COW space.
       cog-atomspace-ro! - Mark AtomSpace as read-only.
")

(set-procedure-property! cog-value? 'documentation
"
 cog-value? EXP
    Return #t if EXP is an OpenCog value, else return #f

    Example:
       guile> (cog-value? (FloatValue 42))
       #t
       guile> (cog-value? 42)
       #f
")

(set-procedure-property! cog-value->list 'documentation
"
 cog-value->list VALUE
    Return a scheme list holding the values in the Atomese VALUE.
    If VALUE is a Link, this returns the outgoing set.
    If VALUE is a Node, this returns list containing the node name.
    If VALUE is a StringValue, FloatValue or LinkValue, this returns
        the associated list of values.

    Example:
       guile> (cog-value->list (FloatValue 0.1 0.2 0.3))
       (0.1 0.2 0.3)
       guile> (cog-value->list (Number 1 2 3))
       (1.0 2.0 3.0)
")

(set-procedure-property! cog-value-ref 'documentation
"
 cog-value-ref VALUE N
 cog-value-ref ATOM KEY N
    The first form returns the N'th entry in the OpenCog VALUE.
    The second form looks up the value om ATOM at KEY, and then returns
    the N'th entry.

    If the value is a Link, this returns the N'th atom in the outgoing set.
        That is, it returns the same atom as cog-outgoing-atom.
    If the value is a Node, and N is zero, this returns the node name.
    If the value is a StringValue, FloatValue or LinkValue, this returns
        the N'th entry in the value.

    The first form returns the same result as
        (list-ref (cog-value->list VALUE) N)

    The second form returns the same result as
        (list-ref (cog-value->list (cog-value ATOM KEY)) N)

    The difference is that this one call will be a lot faster than
    either of the equivalent forms (and thus is suitable for use in
    tight inner loops).

    Example:
       guile> (cog-value-ref (FloatValue 0.1 0.2 0.3) 2)
       0.3
       guile> (cog-value-ref (Number 1 2 3) 2)
       3.0

	See also:
       cog-set-value-ref! - Set one location in a vector.
       cog-inc-value-ref! - Increment one location in a vector.
")

(set-procedure-property! cog-get-types 'documentation
"
 cog-get-types
    Return a list of all of the Atom and Value types in the system.

    Example:
        guile> (cog-get-types)
")

(set-procedure-property! cog-type->int 'documentation
"
 cog-type->int TYPE
    Return the C++ internal type number assigned to an Atom TYPE.
    This provides access to the type numbering in the current C++
    AtomSpace session. The value is not universally unique, and may
    change from one session to the next. This function is for
    internal use only; do not use in general code.
")

(set-procedure-property! cog-get-subtypes 'documentation
"
 cog-get-subtypes TYPE
    Return a list of the subtypes of the given TYPE.  Only the
    immediate subtypes are returned; to obtain all subtypes, this
    function should be called recursively.

    Example:
        guile> (cog-get-subtypes 'Atom)
        (Link Node)
")

(set-procedure-property! cog-subtype? 'documentation
"
 cog-subtype? TYPE SUBTYPE
    Return #t if SUBTYPE is a subtype of TYPE, else return #f.
    The check is performed recursively.

    Example:
        guile> (cog-subtype? 'Node 'Link)
        #f
        guile> (cog-subtype? 'Atom 'Link)
        #t
        guile> (cog-subtype? 'Atom 'ConceptNode)
        #t
")

(set-procedure-property! cog-map-type 'documentation
"
 cog-map-type PROC TYPE [ATOMSPACE]
    Call procedure PROC for each atom in the ATOMSPACE that is of
    type TYPE. If PROC returns any value other than #f, then the
    iteration is terminated.  Note that this iterates only over the
    given type, and not its sub-types. Thus (cog-map-type PROC 'Atom)
    will never call PROC, because no atoms in the AtomSpace can have
    the type Atom: they are all subtypes of Atom.

    The ATOMSPACE argument is optional; if absent, the default
    AtomSpace for this thread is used.

    Example:
       ; define a function that prints the atoms:
       guile> (define (prt-atom h) (display h) #f)
       guile> (cog-map-type prt-atom 'ConceptNode)

  See also: cog-get-atoms TYPE - returns a list of atoms of TYPE.
")

(set-procedure-property! cog-count-atoms 'documentation
"
  cog-count-atoms ATOM-TYPE [ATOMSPACE] -- Count of number of atoms

  Return a count of the number of atoms of the given type `ATOM-TYPE`.
  If the optional argument `ATOMSPACE` is given, then a count is
  returned for that AtomSpace; otherwise, the default AtomSpace for
  this thread is used.

  Example usage:
     (display (cog-count-atoms 'Concept))
  will display a count of all atoms of type 'Concept

  See also:
     cog-get-atoms -- return a list of all atoms of a given type.
     cog-report-counts -- return a report of counts of all atom types.
")

(set-procedure-property! cog-atomspace 'documentation
"
 cog-atomspace [ATOM]
   If the optional ATOM is specified, then return the AtomSpace of ATOM.
   Otherwise, return the current atomspace for this thread.
")

(set-procedure-property! cog-set-atomspace! 'documentation
"
 cog-set-atomspace! ATOMSPACE
    Set the current atomspace for this thread to ATOMSPACE. Every
    thread has it's own current atomspace, to which all atom-processing
    operations apply.  Returns the previous atomspace for this thread.

    Warning: if the previous atomspace is not the primary atomspace
    and is not referenced anywhere, the garbage collector will delete it
    alongside its content, even if some of its content is referenced.
")

(set-procedure-property! cog-atomspace? 'documentation
"
 cog-atomspace? ATOMSPACE
    Return #t if ATOMSPACE is an atomspace; else return #f.
")

(set-procedure-property! cog-new-atomspace 'documentation
"
 cog-new-atomspace [NAME] [ATOMSPACE [ATOMSPACE2 [...]]]
    Create a new atomspace.  If the optional argument NAME is present,
    and it is a string, then the new AtomSpace will be given this
    name. If a list of AtomeSpaces are present, then the new AtomSpace
    will include these as subframes (subspaces).

    Returns the new atomspace.

    AtomSpaces are automatically deleted when no more references to
    them remain.

    Note that this does NOT set the current atomspace to the new one;
    to do that, you need to use cog-set-atomspace!

    The name of the AtomSpace can be obtained with `cog-name`.

    See also:
       cog-atomspace -- Get the current AtomSpace in this thread.
       cog-atomspace-env -- Get the subspaces.
       cog-atomspace-uuid -- Get the UUID of the AtomSpace.
       cog-atomspace-ro! -- Mark the AtomSpace as read-only.
       cog-atomspace-cow! -- Mark the AtomSpace as copy-on-write.
")

(set-procedure-property! cog-atomspace-env 'documentation
"
 cog-atomspace-env [ATOMSPACE]
    Return the parent(s) of ATOMSPACE. The ATOMSPACE argument is
    optional; if not specified, the current atomspace is assumed.

    See also:
       cog-atomspace -- Get the current AtomSpace in this thread.
       cog-atomspace-uuid -- Get the UUID of the AtomSpace.
")

(set-procedure-property! cog-atomspace-uuid 'documentation
"
 cog-atomspace-uuid [ATOMSPACE]
    Return the UUID of ATOMSPACE. The ATOMSPACE argument is
    optional; if not specified, the current atomspace is assumed.

    See also:
       cog-atomspace -- Get the current AtomSpace in this thread.
       cog-atomspace-env -- Get the subspaces.
")

(set-procedure-property! cog-atomspace-clear 'documentation
"
 cog-atomspace-clear [ATOMSPACE]
     Extract all atoms from ATOMSPACE. The ATOMSPACE argument is
     optional; if not specified, the current atomspace is assumed.

     This only removes the atoms from the current atomspace frame,
     it does NOT remove them from any attached storage. Use the
     `delete-frame!` function to remove frames from storage.

     Note that removing a frame may cause Atoms in lower frames to
     become visible! This is because `absent` atoms are used to hide
     Atoms in lower layers; when these are removed, the lower atoms
     become visible.
")

(set-procedure-property! cog-atomspace-ro! 'documentation
"
 cog-atomspace-ro! [ATOMSPACE]
     Mark the ATOMSPACE as being read-only. New atoms cannot be added
     to a read-only atomspace, nor can atoms be removed. The Values
     (including the TruthValues) of atoms in the read-only atomspace
     cannot be changed.

     The ATOMSPACE argument is optional; if not specified, the current
     atomspace is assumed.

     See also: cog-atomspace-rw!, cog-atomspace-readonly?,
         cog-atomspace-cow! and cog-atomspace-cow?
")

(set-procedure-property! cog-atomspace-rw! 'documentation
"
 cog-atomspace-rw! [ATOMSPACE]
     Mark the ATOMSPACE as being read-write. See cog-atomspace-ro!
     for a detailed explanation.

     The ATOMSPACE argument is optional; if not specified, the current
     atomspace is assumed.

     See also: cog-atomspace-readonly?, cog-atomspace-cow! and
         cog-atomspace-cow?
")

(set-procedure-property! cog-atomspace-readonly? 'documentation
"
 cog-atomspace-readonly? [ATOMSPACE]
     Return #t if the ATOMSPACE is marked read-only. See
     cog-atomspace-ro! for a detailed explanation.

     The ATOMSPACE argument is optional; if not specified, the current
     atomspace is assumed.

     See also: cog-atomspace-cow! and cog-atomspace-cow?
")

(set-procedure-property! cog-atomspace-cow! 'documentation
"
 cog-atomspace-cow! BOOL [ATOMSPACE]
     Set the copy-on-write (COW) bit on the ATOMSPACE to BOOL.

     A COW atomspace behaves as if the parent has been marked read-only,
     and so any modifications to atoms in a COW space do not affect the
     parent. (It does not make sense to mark an atomspace as being COW,
     if there is no parent.)

     The ATOMSPACE argument is optional; if not specified, the current
     atomspace is assumed.

     COW spaces are useful as temporary or transient AtomSpaces, so that
     scratch calculations and updates can be performed without affecting
     the parent.

     Long chains of COW spaces can also be used to record a history of
     changes to Values stored on Atoms. This can be useful in several
     ways, including backtracking from complex inferences.

     Atoms that are deleted in COW spaces are removed only in that
     space; they remain in the parent AtomSpaces. They can also be
     added back in, farther up a stack of spaces.

     See also: cog-atomspace-cow?, cog-atomspace-readonly?,
         cog-atomspace-ro! and cog-atomspace-rw!,
")

(set-procedure-property! cog-atomspace-cow? 'documentation
"
 cog-atomspace-cow? [ATOMSPACE]
     Return the copy-on-write (COW) bit on the ATOMSPACE to BOOL.
     See cog-atomspace-cow! for an explanation.

     The ATOMSPACE argument is optional; if not specified, the current
     atomspace is assumed.

     See also: cog-atomspace-ro! and cog-atomspace-rw! and
         cog-atomspace-readonly?,
")

(set-procedure-property! cog-set-server-mode! 'documentation
"
 cog-set-server-mode! BOOL
     If BOOL is #t, then some server-freindly options are enabled,
     including the high-precision printing of TruthValues. Otherwise,
     human-friendly shell-evaluator style is used. The default is
     false. Returns the previous setting.
")

;set-procedure-property! cog-yield 'documentation
;"
; cog-yield
;    The implementation uses a simple exception mechanism to allow
;    scheme code to return to the guile prompt from anywhere. To use
;    this, simply throw 'cog-yield from anywhere.  The catch handler
;    will promptly return to the cogserver.  This can be used with
;    continuations to implement some simple multi-threading.
;
;    Example:
;       guile> (throw 'cog-yield \"hello world\")
;       (hello world)
;")
