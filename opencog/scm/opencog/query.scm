;
; OpenCog Pattern matcher module
;

(define-module (opencog query))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

; This is also loaded by (opencog exec) We need it here,
; else we get undefined symbols in libquery.
(load-extension "libexecution" "opencog_exec_init")

(load-extension "libquery" "opencog_query_init")

; We need to list everything that was already exported by the shared
; library; failure to do so causes warning messages to be printed,
; because other scheme code cannot guess what names the shared lib
; actually exported.  So we list them here.
(export
	cog-bind-first-n
	cog-satisfying-set-first-n
	cog-satisfy
)


(define-public (cog-bind handle)
	(display "Obsolete! Do not use cog-bind, use cog-execute! insead.\n")
	(cog-bind-first-n handle -1)
)
(define-public (cog-bind-single handle)
	(cog-bind-first-n handle 1)
)
(define-public (cog-satisfying-set handle)
	(display "Obsolete! Do not use cog-satisfying-set, use cog-execute! insead.\n")
	(cog-satisfying-set-first-n handle -1)
)
(define-public (cog-satisfying-element handle)
	(cog-satisfying-set-first-n handle 1)
)

(set-procedure-property! cog-bind 'documentation
"
 cog-bind HANDLE
    OBSELETE! DO NOT USE IN NEW CODE! Use cog-execute! instead.

    Run pattern matcher on HANDLE.  HANDLE must be a BindLink.
    Uses crisp (non-probabilistic) logic during the evaluation
    of evaluatable terms.
")

(set-procedure-property! cog-bind-first-n 'documentation
"
 cog-bind-first-n HANDLE
    Run pattern matcher on HANDLE.  HANDLE must be a BindLink.
    The search is terminated after the first N matches are found.
")

(set-procedure-property! cog-bind-single 'documentation
"
 cog-bind-single HANDLE
    Run pattern matcher on HANDLE.  HANDLE must be a BindLink.
    The search is terminated after the first match is found.
")

(set-procedure-property! cog-satisfy 'documentation
"
 cog-satisfy HANDLE
    OBSELETE! DO NOT USE IN NEW CODE! Use cog-evaluate! instead.

    Run pattern matcher on HANDLE.  HANDLE must be a SatisfactionLink.
    Return a TV. Only satisfaction is performed, no implication.
")

; The documentation below belongs in a different module.

(set-procedure-property! cog-value-is-type? 'documentation
"
 cog-value-is-type? TYPE-SPEC VALUE

  Type checker.  Returns true if `VALUE` is of type `TYPE-SPEC`.
  More precisely, returns true if `VALUE` will fit into the type
  specification given by `TYPE-SPEC`; that the value and the type
  specification can be connected. This is usefule for beta-reduction,
  (to check that some argument is reducible) or for pattern matching
  (searching).
")

(set-procedure-property! cog-type-match? 'documentation
"
 cog-type-match? LEFT RIGHT

  Type matcher. Returns true if `LEFT` can mate with `RIGHT`.
  Here, `LEFT` can be a type definition, and `RIGHT` can be
  another type defintion, or a value.  Mating is possible whenever
  `LEFT` is broader, less restricitve than `RIGHT`; equivalently
  if `RIGHT` is narrower than 'LEFT`.

  Mating types and arguments:
  LEFT == (Type 'ConceptNode)    RIGHT == (Concept \"foo\")  can mate.
  LEFT == (Type 'ConceptNode)    RIGHT == (Number 13)  cannot.

  Mating types and types:
  LEFT == (Type 'ConceptNode)    RIGHT == (Type 'ConceptNode)  can mate.

  Left is wider (polymorphic, in this case)
    LEFT == (TypeChoice (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Type 'NumberNode)  can mate.

  Function call arguments can be checked:
    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Concept 'fooNode)  can mate.

    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Number 13)  cannot.

  Function call chains can be checked:
    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Type 'ConceptNode)  can mate.

  The following can mate, because LEFT accepts a concept as input,
  and RIGHT generates a concept as output:
    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Arrow (Type 'EvaluationNode) (Type 'ConceptNode)

  Any type specification is valid: SignatureLinks, etc work too.
")

(set-procedure-property! cog-type-compose 'documentation
"
 cog-type-compose LEFT RIGHT

  Similar to cog-type-match?, but return the composition
  (beta-reduction) of the match. If the types do NOT match, an
  exception is thrown.  If the types do match, then, for many
  cases, the right side is the result.  The compostion of arrows,
  however, results either in a new arrow, or a simple return type.

  Examples:

  Function call arguments can be checked:
    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Concept \"foo\")  can mate.
    result = (Type 'NumberNode)

  Function call chains:
    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Type 'ConceptNode)  can mate.
    result = (Type 'NumberNode)

  The following can mate, because LEFT accepts a concept as input,
  and RIGHT generates a concept as output:
    LEFT == (Arrow (Type 'ConceptNode) (Type 'NumberNode))
    RIGHT == (Arrow (Type 'EvaluationLink) (Type 'ConceptNode)
    result = (Arrow (Type 'EvaluationLink) (Type 'NumberNode))
")
