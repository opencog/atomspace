;
; types.scm
;
; Backwards-compatible type utilities
;
(define-public (cog-type? SYMBOL)
"
 cog-type? SYMBOL
    Return #t if the SYMBOL names a Value or Atom type, else return #f.
    Equivalent to (cog-subtype? 'Value SYMBOL)

    Example:
        guile> (cog-type? 'ConceptNode)
        #t
        guile> (cog-type? 'FlorgleBarf)
        #f

    See also:
        cog-type ATOM -- return the type of ATOM
        cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
        cog-link-type? TYPE -- return #t if TYPE is a Link type
        cog-node-type? TYPE -- return #t if TYPE is a Node type
        cog-value-type? TYPE -- return #t if TYPE is a Value type
"
	(cog-subtype? 'Value SYMBOL)
)

(define-public (cog-node-type? SYMBOL)
"
 cog-node-type? SYMBOL
    Return #t if the SYMBOL names an Node type, else return #f.
    Equivalent to (cog-subtype? 'Node SYMBOL)

    Example:
        guile> (cog-node-type? 'ConceptNode)
        #t
        guile> (cog-node-type? 'ListLink)
        #f
        guile> (cog-node-type? 'FlorgleBarf)
        #f

    See also:
        cog-type? TYPE -- return #t if TYPE is a type
        cog-type ATOM -- return the type of ATOM
        cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
        cog-link-type? TYPE -- return #t if TYPE is a Link type
        cog-value-type? TYPE -- return #t if TYPE is a Value type
"
	(cog-subtype? 'Node SYMBOL)
)

(define-public (cog-link-type? SYMBOL)
"
 cog-link-type? SYMBOL
    Return #t if the SYMBOL names a Link type, else return #f.
    Equivalent to (cog-subtype? 'Link SYMBOL)

    Example:
        guile> (cog-link-type? 'ConceptNode)
        #f
        guile> (cog-link-type? 'ListLink)
        #t
        guile> (cog-link-type? 'FlorgleBarf)
        #f

    See also:
        cog-node-type? TYPE -- return #t if TYPE is a Node type
        cog-type? TYPE -- return #t if TYPE is a type
        cog-type ATOM -- return the type of ATOM
        cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
        cog-value-type? TYPE -- return #t if TYPE is a Value type
"
	(cog-subtype? 'Link SYMBOL)
)

(define-public (cog-value-type? SYMBOL)
"
 cog-value-type? SYMBOL
    Return #t if the SYMBOL names a Value but not an Atom type, else return #f.
    Equivalent to
    (and (cog-subtype? 'Value SYMBOL) (not (cog-subtype? 'Atom SYMBOL)))

    Example:
        guile> (cog-value-type? 'FloatValue)
        #t
        guile> (cog-value-type? 'ConceptNode)
        #f
        guile> (cog-type? 'ConceptNode)
        #t
        guile> (cog-type? 'FlorgleBarf)
        #f

    See also:
        cog-link-type? TYPE -- return #t if TYPE is a Link type
        cog-node-type? TYPE -- return #t if TYPE is a Node type
        cog-type? TYPE -- return #t if TYPE is a type
        cog-type ATOM -- return the type of ATOM
        cog-value-type ATOM KEY -- get the type of the value at KEY on ATOM.
"
	(and (cog-subtype? 'Value SYMBOL) (not (cog-subtype? 'Atom SYMBOL)))
)
