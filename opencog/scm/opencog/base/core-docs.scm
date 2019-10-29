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
(set-procedure-property! cog-new-node 'documentation
"
 cog-new-node NODE-TYPE NODE-NAME
    Create a new node of type NODE-TYPE and name NODE-NAME.

    Optionally, a truth value can follow the node name.

    Throws errors if node-type is not a valid atom type for a node,
    and if node-name is not a string.

    Example:
        ; Create a new node, and prints its value:
        guile> (cog-new-node 'ConceptNode \"some node name\")
        (ConceptNode \"some node name\")

        ; Creates a new node, with a truth value:
        guile> (cog-new-node 'ConceptNode \"another node\"
                      (cog-new-stv 0.8 0.9))
        (ConceptNode \"another node\" (stv 0.8 0.9))
")

(set-procedure-property! cog-node 'documentation
"
 cog-node NODE-TYPE NODE-NAME
    Returns the node of type NODE-TYPE and name NODE-NAME, if it exists,
    else returns null.

    Optionally, a truth value can follow the node name. If the node
    exists, then the truth value is modified.

    Throws errors if node-type is not a valid atom type for a node,
    and if node-name is not a string.

    Example:
        ; Check to see if a node exists:
        guile> (cog-node 'ConceptNode \"asdf\")
        ()

        ; Verify that the return value is actually a true null:
        guile> (null? (cog-node 'ConceptNode \"asdf\"))
        #t

        ; Now, create the node, and see if it exists:
        guile> (cog-new-node 'ConceptNode \"asdf\")
        (ConceptNode \"asdf\")
        guile> (null? (cog-node 'ConceptNode \"asdf\"))
        #f

        ; Change the truth value of an existing node:
        guile> (cog-node 'ConceptNode \"asdf\" (cog-new-stv 0.8 0.9))
        (ConceptNode \"asdf\" (stv 0.8 0.9))
")

(set-procedure-property! cog-new-link 'documentation
"
 cog-new-link LINK-TYPE ATOM-1 ... ATOM-N
    Create a new link, of type LINK-TYPE, with the given atoms in the link.

    Optionally, a truth value can be included in the list of atoms.

    Throws errors if the link type is not a valid opencog link type,
    or if any of the arguments after the link type are not atoms or
    truth values.

    Example:
        ; Creates two nodes, and a new link:
        guile> (define x (cog-new-node 'ConceptNode \"abc\"))
        guile> (define y (cog-new-node 'ConceptNode \"def\"))
        guile> (cog-new-link 'Link x y)
        (Link
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )

        ; Create a new link with a truth value:
        guile> (cog-new-link 'Link x y (cog-new-stv 0.7 0.8))
        (Link (stv 0.7 0.8)
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )
")

(set-procedure-property! cog-link 'documentation
"
 cog-link LINK-TYPE ATOM-1 ... ATOM-N
    Returns the link of the given type LINK-TYPE and list of atoms,
    if it exists, else returns null.

    Optionally, a truth value can be included in the list of atoms.
    If the link exists, then the truth value is modified.

    Throws errors if the link type is not a valid opencog link type,
    or if any of the arguments after the link type are not atoms or
    truth values.

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
        guile> (cog-link 'Link x y (cog-new-stv 0.7 0.8))
        (Link (stv 0.7 0.8)
           (ConceptNode \"abc\")
           (ConceptNode \"def\")
        )
")

(set-procedure-property! cog-delete 'documentation
"
 cog-delete ATOM
    Remove the indicated ATOM from the AtomSpace, but only if it has no
    incoming links. If it has incoming links, the remove fails.  If SQL
    or other data storage is attached, the ATOM is also removed from
    the storage.

    Returns #t if the atom was removed, else returns #f if not removed.

    Use cog-extract to remove from the AtomSpace only, leaving storage
    unaffected.

    Use cog-delete-recursive to force removal of this atom, together
    with any links that might be holding this atom.

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (define y (cog-new-node 'ConceptNode \"def\"))
       guile> (define l (cog-new-link 'Link x y))

       ; Verify that there's an atom called x:
       guile> x
       (ConceptNode \"abc\")

       ; Try to delete x. This should fail, since there's a link
       ; containing x.
       guile> (cog-delete x)
       #f

       ; Delete x, and everything pointing to it. This should delete
       ; both x, and the link l.
       guile> (cog-delete-recursive x)
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

(set-procedure-property! cog-delete-recursive 'documentation
"
 cog-delete-recursive ATOM
    Remove the indicated ATOM from the AtomSpace, and all atoms that
    point at it.  If SQL or other data storage is attached, the ATOM is
    also removed from the storage.

    Return #t on success, else return #f if not removed.
")

(set-procedure-property! cog-extract 'documentation
"
 cog-extract ATOM
    Remove the indicated ATOM from the AtomSpace, but only if it has no
    incoming links. If it has incoming links, the remove fails.

    Returns #t if the atom was removed, else returns #f if not removed.

    This does NOT remove the atom from any attached storage (e.g. SQL
    storage).  Use cog-delete to remove from atoms from storage.

    Use cog-extract-recursive to force removal of this atom, together
    with any links that might be holding this atom.

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (define y (cog-new-node 'ConceptNode \"def\"))
       guile> (define l (cog-new-link 'Link x y))

       ; Verify that there's an atom called x:
       guile> x
       (ConceptNode \"abc\")

       ; Try to extract x. This should fail, since there's a link
       ; containing x.
       guile> (cog-extract x)
       #f

       ; Delete x, and everything pointing to it. This should extract
       ; both x, and the link l.
       guile> (cog-extract-recursive x)
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

(set-procedure-property! cog-extract-recursive 'documentation
"
 cog-extract-recursive ATOM
    Remove the indicated ATOM from the AtomSpace, and all atoms that
    point at it.  Return #t on success, else return #f if not removed.

    The atom is NOT removed from SQL or other attached data storage.
    If you need to delete from storage, use cog-delete and
    cog-delete-recursive.
")

(set-procedure-property! cog-atom? 'documentation
"
 cog-atom? EXP
    Return #t if EXP is an atom, else return #f

    Example:
       ; Define a node
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
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
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (define y (cog-new-link 'ListLink x))
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
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (define y (cog-new-link 'ListLink x))
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
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (cog-name x)
       \"abc\"
")

(set-procedure-property! cog-number 'documentation
"
 cog-number NUMBER-NODE
    Return the floating point value of the NumberNode NUMBER-NODE.
    If it is a NumberNode, then this is the same as saying
        (string->number (cog-name NUMBER-NODE))
    If it is not a NumberNode, then this will throw an exception.

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

    Example:
       ; Define a node
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (cog-type x)
       ConceptNode
       guile> (eq? 'ConceptNode (cog-type x))
       #t
")

(set-procedure-property! cog-arity 'documentation
"
 cog-arity ATOM
    Return the arity of ATOM.

    Example:
       guile> (define x (cog-new-node 'ConceptNode \"abc\"))
       guile> (cog-arity x)
       0
       guile> (define l (cog-new-link 'Link x x x))
       guile> (cog-arity l)
       3
")

(set-procedure-property! cog-incoming-set 'documentation
"
 cog-incoming-set ATOM
    Return the incoming set of ATOM.  This set is returned as an
    ordinary scheme list.

    See also: cog-incoming-size, cog-incoming-by-type
    Example:
       ; Define two nodes and a link between them:
       guile> (define x (ConceptNode \"abc\"))
       guile> (define y (ConceptNode \"def\"))
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
 cog-incoming-size ATOM
    Return the number of atoms in the incoming set of ATOM.

    See also: cog-incoming-set, cog-incoming-size-by-type

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (ConceptNode \"abc\"))
       guile> (define y (ConceptNode \"def\"))
       guile> (Link x y)

       ; Get the size of the incoming set of nodes x and y:
       guile> (cog-incoming-size x)
       => 1

       guile> (cog-incoming-size y)
       => 1
")

(set-procedure-property! cog-incoming-by-type 'documentation
"
 cog-incoming-by-type ATOM TYPE
    Return the incoming set of ATOM that consists only of atoms of
    type TYPE.  This set is returned as an ordinary scheme list.

    Equivalent to (cog-filter TYPE (cog-incoming-set ATOM)), but
    should be faster, performance-wise.

    Example:
       ; Define two nodes and two links between them:
       guile> (define x (ConceptNode \"abc\"))
       guile> (define y (ConceptNode \"def\"))
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
 cog-incoming-size-by-type ATOM TYPE
    Return the number of atoms of type TYPE in the incoming set of ATOM.

    See also: cog-incoming-by-type, cog-incoming-size

    Example:
       ; Define two nodes and a link between them:
       guile> (define x (ConceptNode \"abc\"))
       guile> (define y (ConceptNode \"def\"))
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
    from the component parts of the atom (but not it's values), that
    can be used in hash tables or other algorithms that require a hash.

    Example:
       guile> (cog-handle (Concept \"abc\"))
       999283543311182409
")

(set-procedure-property! cog-atom-less? 'documentation
"
 cog-atom-less? L-ATOM R-ATOM
    Return #t if L-ATOM is less than R-ATOM, else return #f.  This
    predicate is usefule for creating sorted lists of atoms; for
    example, to rapidly remove duplicate atoms from a long list.

    Example:
       guile> (cog-atom-less? (Concept \"abc\") (Concept \"def\"))
       #f
")

(set-procedure-property! cog-inc-count! 'documentation
"
  cog-inc-count! ATOM CNT -- Increment count truth value on ATOM by CNT.

  Increment the count on a CountTruthValue by CNT. The mean and
  confidence values are left untouched.  CNT may be any floating-point
  number (positive or negative).

  If the current truth value on the ATOM is not a CountTruthValue,
  then the truth value is replaced by a CountTruthValue, with the
  count set to CNT.

  Example usage:
     (cog-inc-count! (ConceptNode \"Answer\") 42.0)

  See also: cog-inc-value! for a generic version
")

(set-procedure-property! cog-inc-value! 'documentation
"
  cog-inc-value! ATOM KEY CNT REF -- Increment value on ATOM by CNT.

  The REF location of the FloatValue at KEY is incremented by CNT.
  CNT may be any floating-point number (positive or negative).
  The rest of the FloatValue vector is left untouched.

  If the ATOM does not have any Value at KEY, or if the current Value
  is not a FloatValue, then a new FloatValue of length (REF+1) is
  created. If the existing FloatValue is too short, it is extended
  until it is at least (REF+1) in length.

  Example usage:
     (cog-inc-value!
         (ConceptNode \"Question\")
         (PredicateNode \"Answer\")
         42.0  0)

  See also: cog-inc-count! for a version that increments the count TV.
")

(set-procedure-property! cog-mean 'documentation
"
 cog-mean ATOM
    Return the `mean` of the TruthValue on ATOM. This is a single
    floating point-number.
")

(set-procedure-property! cog-confidence 'documentation
"
 cog-confidence ATOM
    Return the `confidence` of the TruthValue on ATOM. This is a single
    floating point-number.
")

(set-procedure-property! cog-count 'documentation
"
 cog-count ATOM
    Return the `count` of the TruthValue on ATOM. This is a single
    floating point-number.
")

; ===================================================================

(set-procedure-property! cog-tv 'documentation
"
 cog-tv ATOM
    Return the truth-value of ATOM.

    Example:
       ; Define a node
       guile> (define x
                 (cog-new-node 'ConceptNode \"abc\"
                    (cog-new-stv 0.2 0.5)))
       guile> (cog-tv x)
       (stv 0.2 0.5)
       guile> (cog-tv? (cog-tv x))
       #t
")

(set-procedure-property! cog-set-tv! 'documentation
"
 cog-set-tv! ATOM TV
    Set the truth-value of ATOM to TV.

    Example:
       ; Define a node
       guile> (define x (cog-new-node 'ConceptNode \"def\"))
       guile> (cog-tv x)
       (stv 0 0)
       guile> (cog-set-tv! x (cog-new-stv 0.9 0.8))
       (ConceptNode \"def\" (stv 0.9 0.8))
       guile> (cog-tv x)
       (stv 0.9 0.8)
")

(set-procedure-property! cog-tv-mean 'documentation
"
 cog-tv-mean TV
    Return the `mean` of the TruthValue TV. This is a single
    floating point-number.
")

(set-procedure-property! cog-tv-confidence 'documentation
"
 cog-tv-confidence TV
    Return the `confidence` of the TruthValue TV. This is a single
    floating point-number.
")

(set-procedure-property! cog-tv-count 'documentation
"
 cog-tv-count TV
    Return the `count` of the TruthValue TV. This is a single
    floating point-number.
")

; ===================================================================
;

(set-procedure-property! cog-new-value 'documentation
"
 cog-new-value TYPE LIST
    Create a new value of type TYPE, hold the LIST of strings, floats
    or values.  The TYPE must be either 'StringValue, 'FloatValue
    or 'LinkValue. The LIST must be an ordinary guile list, consisting
    entirely of guile strings, guile numbers, or opencog values,
    respectively, for each of the three types.

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
")

(set-procedure-property! cog-value 'documentation
"
 cog-value ATOM KEY
    Return the value of of KEY for ATOM. Both ATOM and KEY must be
    atoms.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-value (Concept \"abc\") (Predicate \"key\"))
       (FloatValue 1.000000 2.000000 3.00000)
")

(set-procedure-property! cog-set-value! 'documentation
"
 cog-set-value! ATOM KEY VALUE
    Set the value of KEY for ATOM to VALUE. Both ATOM and KEY must be
    atoms.

    Example:
       guile> (cog-set-value!
                 (Concept \"abc\") (Predicate \"key\")
                 (FloatValue 1 2 3))
       guile> (cog-value (Concept \"abc\") (Predicate \"key\"))
       (FloatValue 1.000000 2.000000 3.00000)
")

(set-procedure-property! cog-value? 'documentation
"
 cog-value? EXP
    Return #t if EXP is an opencog value, else return #f

    Example:
       guile> (cog-value? (FloatValue 42))
       #t
       guile> (cog-value? 42)
       #f
")

(set-procedure-property! cog-value->list 'documentation
"
 cog-value->list VALUE
    Return a scheme list holding the values in the opencog VALUE.
    If VALUE is a Link, this returns the outgoing set.
    If VALUE is a Node, this returns list containing the node name.
    If VALUE is a StringValue, FloatValue or LinkValue, this returns
        the associated list of values.

    Example:
       guile> (cog-value->list (FloatValue 0.1 0.2 0.3))
       (0.1 0.2 0.3)
")

(set-procedure-property! cog-value-ref 'documentation
"
 cog-value-ref VALUE N
    Return the N'th entry in the opencog VALUE.
    If VALUE is a Link, this returns the N'th atom in the outgoing set.
        That is, it returns the same atom as cog-outgoing-atom.
    If VALUE is a Node, and N is zero, this returns the node name.
    If VALUE is a StringValue, FloatValue or LinkValue, this returns
        the N'th entry in the value.

    This returns the same result as
        (list-ref (cog-value->list VALUE) N)

    Example:
       guile> (cog-value-ref (FloatValue 0.1 0.2 0.3) 2)
       0.3
")

(set-procedure-property! cog-as 'documentation
"
 cog-as ATOM
    Return the atomspace of the ATOM.  If the ATOM does not belong to
    any atomspace, null is returned.
")

(set-procedure-property! cog-get-types 'documentation
"
 cog-get-types
    Return a list of all of the atom and value types in the system.

    Example:
        guile> (cog-get-types)
")

(set-procedure-property! cog-type->int 'documentation
"
 cog-type->int TYPE
    Return the integer value corresponding to an atom TYPE.

    Example:
        guile> (cog-type->int 'ListLink)
        8
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
 cog-map-type PROC TYPE
    Call proceedure PROC for each atom in the atomspace that is of
    type TYPE. If proc returns any value other than #f, then the
    iteration is terminated.  Note that this iterates only over the
    given type, and not its sub-types. Thus (cog-map-type proc 'Atom)
    will never call proc, because no atoms in the atomspace can have
    the type Atom: they are all subtypes of Atom.

    Example:
       ; define a function that prints the atoms:
       guile> (define (prt-atom h) (display h) #f)
       guile> (cog-map-type prt-atom 'ConceptNode)

  See also: cog-get-atoms TYPE - returns a list of atoms of TYPE.
")

(set-procedure-property! cog-count-atoms 'documentation
"
  cog-count-atoms -- Count of the number of atoms of given type

  cog-count-atoms ATOM-TYPE
  Return a count of the number of atoms of the given type `ATOM-TYPE`.

  Example usage:
     (display (cog-count-atoms 'ConceptNode))
  will display a count of all atoms of type 'ConceptNode
")

(set-procedure-property! cog-atomspace 'documentation
"
 cog-atomspace
     Return the current atomspace for this thread.
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
 cog-new-atomspace [ATOMSPACE]
    Create a new atomspace.  If the optional argument ATOMSPACE
    is present, then the new atomspace will be an expansion (child)
    of ATOMSPACE.  Atomspaces are automatically deleted when no more
    references to them remain. Returns the new atomspace.

    Note that this does NOT set the current atomspace to the new one;
    to do that, you need to use cog-set-atomspace!
")

(set-procedure-property! cog-atomspace-env 'documentation
"
 cog-atomspace-env [ATOMSPACE]
     Return the parent of ATOMSPACE. The ATOMSPACE argument is
     optional; if not specified, the current atomspace is assumed.
")

(set-procedure-property! cog-atomspace-uuid 'documentation
"
 cog-atomspace-uuid [ATOMSPACE]
     Return the UUID of ATOMSPACE. The ATOMSPACE argument is
     optional; if not specified, the current atomspace is assumed.
")

(set-procedure-property! cog-atomspace-clear 'documentation
"
 cog-atomspace-clear [ATOMSPACE]
     Extract all atoms from ATOMSPACE. The ATOMSPACE argument is
     optional; if not specified, the current atomspace is assumed.

     This only removes the atoms from the atomspace, it does NOT
     remove them from the backingstore.
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
