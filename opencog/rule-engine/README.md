# Unified rule engine

## Introduction

The unified rule engine (URE) project aims at building a generic opencog
rule engine on top of the Pattern Matcher, applicable to rules written
in a scheme representation, such as for PLN and R2L. The main components
of the URE includes a forward chainer and a backward chiainer. This will enable
the usage of Backward and Forward chaining inferences.  At the moment,
these rules are written as Pattern Matcher's BindLink, though that can
be changed in the future as these rules are not applied as strict
Pattern Matching query.

Rules are organized inside a "Rule Base", which customizable control
policy for controlling the inferences (such as the number of steps, 
the weight of a rule, etc).

The overall design can be found on the wiki pages below:

  [http://wiki.opencog.org/w/Unified_Rule_Engine](http://wiki.opencog.org/w/Unified_Rule_Engine)

  [http://wiki.opencog.org/w/Control_policy](http://wiki.opencog.org/w/Control_policy)

  [http://wiki.opencog.org/w/Pattern_Matcher](http://wiki.opencog.org/w/Pattern_Matcher)


### Forward chaining (Sept 2015)

#### How to call the forward chainer from a scheme interface?

One can use the `(cog-fc *source* *rule-base* *focus-set*)` scheme binding
to start forward chaining.

*source* - Could be one of the follow:
 - An empty [SetLink](http://wiki.opencog.org/wikihome/index.php/SetLink)
 - A single atom
 - A set of atoms wrapped in a SetLink

When the source is an empty SetLink, forward chainer will apply all rules
leaving aside source and rule selection steps on the specified focus set or on entire atomspace based on the size of the focus set as described below.

*rule-base* - Is a [ConceptNode](http://wiki.opencog.org/wikihome/index.php/ConceptNode) with a particular name describing the rule base.

*focus-set* - A set of atoms wrapped in a SetLink.If the SetLink is not empty,
the forward chainer will apply selected rules on the atoms inside the focus set.otherwise rules will be applied on the entire atomspace.

When both source and focus set are empty, all rules on the whole atomspace will be applied iteratively.

**Example**: suppose there is some knowledges about the ConceptNode
Socrates then one can do a bunch of forward chaining inference by
calling `(cog-fc (ConceptNode "Socrates") (ConceptNode "rb-pln") (SetLink [ATOMS_ASSOCIATED]))`
from the scheme shell interface. All results of the inferences are returned wrapped in a ListLink.

### Backward chaining

In the backward chaining inference we are interested in either truth
value fulfillment query or variable fulfillment query.  For variable
fullfillment query, variable containing link is passed as an argument
and the backward chainer tries to find grounding for the variable.
For truth value fullfillment query, the TV of the original target are
updated via inference.

The main C++ entry point for the backward chainer is the `do_chain` function.

There exist a scheme primitive `(cog-bc *target* *rule-base* *focus-set*)`
for using the Backward Chainer in scheme.

Here's how the criminal example located at
https://github.com/opencog/opencog/blob/master/opencog/python/pln_old/examples/backward_chaining/criminal.scm
could be solved by the Backward Chaining, when only the Modus
Ponens rule is present (disclaminer: the internal implement will
be different)


```
t: InhLink $who criminal
-> kb matched

t: InhLink $x crimainl, InhLink $who criminal
-> kb match fail
-> match modus ponens rule
-> output matched: VarNode $B-1 => InhLink $x criminal
-> input became: (AndLink (ImpLink (VarNode $A-1) (QuoteLink (InhLink $x criminal))) (VarNode $A-1))
-> premises selection
-> none of the premises can be grounded to solve for $x
-> add to targets

t: (ImpLink (AndLink ... american ... weapon ...) (InhLink $x criminal)), (AndLink ... american ... weapon ...), InhLink $x crimnal
-> no free var

t: (AndLink ... american ...), InhLink $x criminal, InhLink $who criminal
-> kb match fail
-> break apart the AndLink

t: InhLink $x American, InhLink $y weapon, EvaLink sell $x $y $z, InhLink $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb matched

t: InhLink $y weapon, EvaLink sell $x $y $z, InhLink $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb matched

t: EvaLink sell $x $y $z, InhLink $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb match sell West $a Nono
-> got free var, add to target

t: EvaLink sell West $a Nono, InhLink $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb match fail
-> match modus ponens rule
-> output matched: VarNode $B-2 => EvaLink sell West $a Nono
-> input became: (AndLink (ImplicationLink (VarNode $A-2) (QuoteLink (EvaLink sell West $a Nono))) (VarNode $A-2))
-> premises selection
-> one of the premises can be grounded by missile@123
-> forward chain added (EvaLink sell West missle@123 Nono) to atomspace
-> no premises with free var, this target is solved

t: InhLink $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb match

t: InhLink $b hostile, InhList $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb match fail
-> match modus ponens rule
-> output matched: VarNode $B-3 => InhLink $b hostile
-> input became: (AndLink (ImplicationLink (VarNode $A-3) (QuoteLink (InhLink $b hostile))) (VarNode $A-3))
-> premises selection
-> one of the premises can be grounded by Nono
-> forward chain added (InhLink Nono hostile) to atomspace
-> no premises with free var, this target is solved

t: InhList $z hostile, InhLink $x criminal, InhLink $who criminal
-> kb match

t: InhLink $x criminal, InhLink $who criminal
-> kb match fail
-> matched modus poenes rule
-> output matched: VarNode $B-4 => InhLink $x criminal
-> input became: (AndLink (ImpLink (VarNode $A-4) (QuoteLink (InhLink $x criminal))) (VarNode $A-4))
-> premises selection
-> one of the premises can be grounded by West
-> forward chain added (InhLink West criminal) to atomspace
-> no premises with free var, this target is solved

t: InhLink $who criminal
-> kb match

$who in the end map to West

```

where `t` is a targets stack (left is the front).  In the actual
implmentation, a list is used and the targets are visited in
some roulette selection way.

## Control policy

The control policy is directly defined in the the AtomSpace according
to the following
[format](http://wiki.opencog.org/w/URE_Configuration_Format). The node
representing the rule-based system (rules + other parameters) is
passed to the chainers (forward or backward) and loaded at
construction time.

## Things need to be implemented

The rule engine as it exists now is in its infancy. There is
a lot of space for improvement.

* Rule choosing fitness functions

* Inference termination

* Refactoring out some codes

* Rules output need to be clearly defined for backward chaining,
  which is not currently possible if the output in hidden
  inside some scheme function

* VariableNode need to have the following properties for backward chaining

  1. all usage of VariableNode are unique & well-defined, in that
     the same named VariableNode is never declared and appears in more than one scope
  2. atoms like `(SatisfyingSetLink (VariableNode $X) (humans eat $X))`,
     `(SatisfyingSetLink (VariableNode $Y) (humans eat $Y))` are treated as exactly the same atom

  This can be done via "canonical label" of the scoping links.  See
  discussion at https://groups.google.com/forum/#!topic/opencog/dKCYL47fpCQ
 
* Both Forward Chainer and Backward Chainer need to handle inferences
  involving existing variables.
  
  For example, given
  ```
   SatisfyingSetLink
     X, Y
     AndLink
       InheritanceLink X Y
       InhertianceLink Y animals
  ```
  
  Then the deduction rule should produce
  ```
   SatisfyingSetLink
     Z
     InheritanceLink Z animals
  ```
  with new variables, instead of the useless `InheritanceLink X animals` that is
  not inside the original scope.
  
  In addition, the premises should all be contained within the same scope, so 
  if the following exist
  ```
   SatisfyingSetLink
     X
     AndLink
       InheritanceLink X animal
       ...

   BindLink
     Y
     AndLink
       InheritanceLink animal Y
       ...
     ...
  ```
  We do not want any form of `InheritanceLink X Y` to be generated.  Special
  care is also needed for nested scopes.

## Rule represenation next steps

- See if these can be implemented to directly use the "side-effect
  free" versions so that the truth value application occurs inside the
  ImplicationLink rather than inside the Scheme rule. This was
  discussed [here](https://groups.google.com/d/msg/opencog/KUptHRvBXu0/YR6oySxLKeMJ).

- See if the link type can be made to allow a dynamic list of valid
  link types. For example, for the Deduction Rule: {InheritanceLink,
  SubsetLink, ImplicationLink, ExtensionalImplicationLink}

- Support all the TruthValue types

- Utilize a graph rewriting unit test framework, that is currently
  being discussed, to assert that the replacement graphs match a
  predefined expected value for specific test instances


***Author*** *Misgana Bayetta*, *William Ma*
