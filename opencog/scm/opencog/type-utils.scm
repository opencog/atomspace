;
; OpenCog type utilities module
;

(define-module (opencog type-utils))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-type-utils "libtype-utils") "opencog_type_utils_init")

; We need to list everything that was already exported by the shared
; library; failure to do so causes warning messages to be printed,
; because other scheme code cannot guess what names the shared lib
; actually exported.  So we list them here.
(export
	cog-value-is-type?
	cog-type-match?
	cog-type-compose
)

(set-procedure-property! cog-value-is-type? 'documentation
"
 cog-value-is-type? TYPE-SPEC VALUE

  Type checker.  Returns true if `VALUE` is of type `TYPE-SPEC`.
  More precisely, returns true if `VALUE` will fit into the type
  specification given by `TYPE-SPEC`; that the value and the type
  specification can be connected. This is useful for beta-reduction,
  (to check that some argument is reducible) or for pattern matching
  (searching).

  Example:
      (cog-value-is-type? (Type 'ConceptNode) (Concept \"foo\"))
      (cog-value-is-type? (Type 'PredicateNode) (Concept \"foo\"))
")

(set-procedure-property! cog-type-match? 'documentation
"
 cog-type-match? LEFT RIGHT

  Type matcher. Returns true if `LEFT` can mate with `RIGHT`.
  Here, `LEFT` can be a type definition, and `RIGHT` can be
  another type definition, or a value.  Mating is possible whenever
  `LEFT` is broader, less restricitve than `RIGHT`; equivalently
  if `RIGHT` is narrower than 'LEFT`.

  Example:
       (cog-type-match? (Type 'PredicateNode) (Type 'ConceptNode))

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
