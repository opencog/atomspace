
Execution Support
=================
The code here implements the Atomese conception of a "function call".
These come in three basic forms: a GroundedSchema or GroundedPredicate,
which can be used to call scheme, python or functions in shared libs.
The second form are the DefinedSchema or DefinedPredicate atoms, which
are functions written in Atomese. The third form are the bare
LambdaLinks, which are just anonymous functions (the DefinedSchema or
DefinedPredicate will usually just be wrappers around a LambdaLink,
giving it a "name" os that its not anonymous any more.)

For the LambdaLinks, the arguments are "plugged into" the Lambda
(beta-reduced), and then the resulting expression is evaluated as
a pure Atomese expression. For Grounded variants, the arguments
are passed to the Grounded handlers for each specific language.

The `EvaluatableLink` contains some historical cruft that could be
eventually cleaned up.
