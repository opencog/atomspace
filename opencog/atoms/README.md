
Atoms
=====

This directory contains C++ implementations of certain atom types.
These atoms are thus "special". The only reason for this code is to
improve performance of the system.  That is, the code here "memoizes"
(i.e. caches) some precomputed values of one sort or another.

Subdirectories
--------------
 * bind -- BindLink, LambdaLink, SatisfactionLink, SatisfactionSetLink,
   GetLink

 * core -- those C++ atoms that do not depend on other C++ subsystems
   e.g. do not depend on scheme or python.

 * execute -- "black-box" executable/evaluatable links, e.g.
   ExecutionOutputLink, EvaluationLink, GroundedPredicateNode, etc.

 * reduct -- inspired by comboreduct, these are "clearbox" links:
   PlusLink, TimesLink, etc.

