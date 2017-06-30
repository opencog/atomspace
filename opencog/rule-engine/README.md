# Unified rule engine

## Introduction

The unified rule engine (URE) is a generic opencog rule engine mostly
built on top of the Pattern Matcher, applicable to rules written in a
scheme/atomese representation, such as for PLN and R2L. The main
components of the URE includes a forward chainer and a backward
chainer. This enables the usage of Backward and Forward chaining
inferences.  At the moment, these rules are written as Pattern
Matcher's BindLink, though that can be changed in the future as these
rules are not applied as strict Pattern Matching query.

Rules are organized inside a "Rule Base", which customizable control
policy for controlling the inferences (such as the number of steps, 
the weight of a rule, etc).

The overall design can be found on the wiki pages below:

  [Unified Rule Engine](http://wiki.opencog.org/w/Unified_Rule_Engine)
  [URE Configuration Format](http://wiki.opencog.org/w/URE_Configuration_Format)
  [URE Control Policy](http://wiki.opencog.org/w/URE_Control_Policy)
  [Pattern Matcher](http://wiki.opencog.org/w/Pattern_Matcher)

Examples can be found in this and other repositories:

  [Rule Engine Examples](https://github.com/opencog/atomspace/tree/master/examples/rule-engine)
  [PLN Rules](https://github.com/opencog/opencog/tree/master/opencog/pln)
  [PLN Examples](https://github.com/opencog/opencog/tree/master/examples/pln)
  [R2L Rules](https://github.com/opencog/opencog/tree/master/opencog/nlp/relex2logic/rules)

### Forward chaining

#### How to call the forward chainer from a scheme interface?

One can use the `(cog-fc *rule-base* *source* *#:vardecl* *#:focus-set*)`
scheme binding to start forward chaining.

*rule-base* - Is a
 [ConceptNode](http://wiki.opencog.org/wikihome/index.php/ConceptNode)
 with a particular name describing the rule base. See [URE_Configuration_Format](http://wiki.opencog.org/w/URE_Configuration_Format).

*source* - Could be one of the follow:
 - An empty [SetLink](http://wiki.opencog.org/wikihome/index.php/SetLink)
 - A single atom
 - A set of atoms wrapped in a SetLink

When the source is an empty SetLink, forward chainer will apply all
rules leaving aside source and rule selection steps on the specified
focus set or on entire atomspace based on the size of the focus set as
described below.

*vardecl* - Optional argument
 - A VariableNode, VariableList or TypedVariable

*focus-set* - Optional argument A set of atoms wrapped in a
SetLink. If the SetLink is not empty, the forward chainer will apply
selected rules on the atoms inside the focus set, otherwise rules will
be applied on the entire atomspace.

When both source and focus set are empty, all rules on the whole atomspace will be applied iteratively.

**Example**: suppose there is some knowledges about the ConceptNode
Socrates then one can do a bunch of forward chaining inference by
calling

```scheme
(cog-fc (ConceptNode "rb-pln") (ConceptNode "Socrates") #:focus-set (SetLink [ATOMS_ASSOCIATED]))
```

from the scheme shell interface. All results of the inferences are
returned wrapped in a ListLink.

### Backward chaining

In the backward chaining inference we are interested in either truth
value fulfillment query or variable fulfillment query.  For variable
fulfillment query, variable containing link is passed as an argument
and the backward chainer tries to find grounding for the variable.
For truth value fullfillment query, the TV of the original target are
updated via inference.

The main C++ entry point for the backward chainer is the `do_chain`
function.

Similarly to the Forward Chainer there exist a scheme primitive
`(cog-bc *rule-base* *target* *#:vardecl* *#:focus-set*)` for calling the
Backward Chainer in scheme. The arguments are the same as for a
Forward Chainer call expect that *source* is replaced by *target*.

## Control policy

The control policy is directly defined in the the AtomSpace according
to the following
[format](http://wiki.opencog.org/w/URE_Configuration_Format). The node
representing the rule-based system (rules + other parameters) is
passed to the chainers (forward or backward) and loaded at
construction time.

## Improvements

The rule engine is rather mature at this point but there are still a
few missing things.

* The backward chainer needs to better support meta-rule. For now
  meta-rules in the backward chainer as run forwardly at each
  iteration and all new produced rules are added to the rule
  sets. Instead if should build an inference tree that directly call
  meta-rules. Because of this limitation some reasonings, like
  induction of conditionals are not possible in a purely backward
  way. In order to do that we need to integrate unification in atomese
  so that the inference tree process meta-rules.

* Add more inference termination criteria besides maximum number of
  iterations.

* The unfier needs to support better type intersection. For now if a
  variable is typed in a variable fulfillment query, and a rule
  unifies with this variables with another type, the resulting type
  will be one or the other, as opposed to being its intersection. At
  worse it invalidates some backward chainer results, at beast it
  slows down inference tree execution.

## Authors

Misgana Bayetta, William Ma, Nil Geisweiller.

## Ackowledgement

Many thanks to Jim Rutt for supporting the rule-engine developement.
