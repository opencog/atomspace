
Network Inference and Analysis Tools
====================================

In this project, there's a generic theme of infering structure from
a sequence of events.  That is, a sequence of events is observed in the
external qorld, and the question arises: are these events correlated?
Do they mean something when observed together, or is it happenstance?
What is the relationship between the items in the sequence of events?

Theoretical computer science has explored a number of theories for
describing the relationships between ordered events; these theories
are inter-related, and go under the name of:

 * sequent calculus (proof theory)
 * process caluclus
 * calculus of communicating systems (CCS)
 * the theory of communicating sequential processes (CSP)
 * history monoids
 * trace monoids and trace theory
 * Actor model
 * Dependency grammar

See the respective Wikipedia articles on each of these topics.

The general theme is that there are a number of distinct actors, each
sending messages to one-another.  This can be viewed as a network, with
edges connecting actors, and messages passing along those edges. These
messages can be observed as sequences of events in time. Sometimes, the
order in a sequence is important, and sometimes it is not. Viz.,
sometimes things "happen at the same time", for no reason, and sometimes
there is a cause-and-effect relationship.

Cause and effect
----------------
Note that, in such systems, the observed "cause" can sometimes come after
the "effect".  For example: to build a high-rise building, a foundation
must be dug first.  Observed as events in time, the construction comes
after the foundation is built: it would be incorrect to say that a hole
in the ground "causes" a sky-scraper to appear.  This is because the
formal cause of the skyscraper is the will of a real-estate developer;
however, this will is not observed; only the construction events are.
From the point of view of the network analysis being done here, the
skyscraper should be viewed as the "head", and the hole in the ground
as the "dependent", with the head dominating the dependent in a
dependency grammar.

Dependency Grammar
------------------
The skyscraper construction analysis provides a reasonable example of
the type of network being analyzed here. The dependency grammar captures
the relationship between a sequence of events: namely some events must
occur before others, some are necessarily after, and sometimes, many
things have to be accomplished at roughly the same time, before the
later stages can take place.  This can be analyzed as a directed graph
or network, with head-dependent relationships.  These relationships
form a grammar, a "dependency grammar" (DG).  The prototypical example
of a dependency grammar comes from linguistics, and describes the
relationship between sequences of words; it need not be words, the
generic idea is generic.

Grammar Inference
-----------------
After observing a large number of skyscraper construction jobs, one can
eventually discern the general relationship between construction events.
One can view these events as being the outcome of "messages" passed
between "actors". The grammar is the set of interlocks displayed on a
Gantt chart showing the dependencies in the construction schedule.
The goal of grammar inference is to discern a grammar, given a large
number of observations of a sequence of events.

The prototyical example, at this point, is from linguistics: the
inference of a natural language grammar from the observation of a
sequence of sentences.

MST parsing
-----------
The primary tool in this directory is an MST parser. Given a specific
sequence of events, viz a sequence of atoms, all of the same type, and
given a large pool of observed dependencies between pairs of events, the
MST parser will construct a dependency tree such that the score of the
edges of the dependency tree are maximized.

Typical pair-wise relationships might be indicated as follows, in the
atomspace:
```
    EvaluationLink   (MI=24.3189)
        PredicateNode "word-pair"
        ListLink
            WordNode "foo"
            WordNode "bar"
```
which indicates that the word-pair (foo,bar) was observed with a mutual
information (MI) of 24.3189.

The atomspace can hold sparse matrix of such pair-wise data; in a
certain sense, the atomspace was designed from the get-go to do exactly
that.

The MST parse just creates a tree connecting all of the atoms in a
sequnce, such that the sum-total (addition) of the scores of all the
edges in the tree is maximal, as compared to any other tree.

Disjuncts
---------
Each vertex in the tree can have one or more edges attached to it. The
edges are called "links".  Each "link" can be viewed as the union of two
"connectors", with one connector coming from one vertex, and the other
connector coming from the other vertex. When these two connectors come
together, they form a link.  The sequnce of connectors attached to a
vertex is called a "disjunct".  The vertex itself, otherther with it's
disjunct is called a "connector set".  These are all concepts stolen
from Link Grammar, where they are applied in a linguistic context;
however, these concepts are generic; thus, the code here attempts to be
generic.
