
Sheafs and Graph/Network Inference
==================================
This directory provides a general overview of sheaf concepts, as they
apply to graph theory and how they are used to represent structure in
Atomese. The `attic` directory also contains an early implementation of
these concepts.  It worked (still works), but a superior implementation
can be found in the [opencog/learn](https://github.com/opencog/learn)
project. Alas, even that implementation has some serious issues' an even
more general approach is being attempted in multiple opencog git repos.

Sheaves - Informal Overview
===========================
Sheaves provide a way for having both embedding vectors and also
symbolic structural components, and thus provide a natural setting for
neuro-symbolic computing.

The starting point for creating and understanding sheaves is a graph,
consisting of vertexes and edges. Take a graph, any graph, pick a vertex,
any vertex, and cut all the edges joining to it. If you cut so that
half-edges remain, with the cut endpoints marked with a type, then it
can always be reconnected back.

The half-cut-edges, when marked with a type, resemble a "jigsaw puzzle
piece". This analogy can be seen explicitly in the 1991 Link Grammar
papers (see below); diagrams showing actual jigsaw pieces appear in
those texts. Jigsaw connectors are a generic concept that is often
employed to create diagrams in tensor algebras, and to solve problems
in tensor logic. (See Stay & Baez paper, below.)

What makes all this a "sheaf" is that any collection of half-assembled
jigsaws, with any collection of still-unattached half-edges
("connectors") is ... just like a single piece -- just a "thing" with
unconnected connectors. This behaves much like jigsaw puzzles in real
life: a block of 2 or 3 connected jigsaw pieces behaves as it if was
just one larger piece. You could glue them together, and never know
that it wasn’t just one large piece.

In math, "sheaf theory" defines 4 axioms for what it means to be a
"sheaf", and it boils down to just "a collection of half-assembled
things with connectors that you can continue to connect however they
allow, as long as the tabs fit correctly". Unfortunately, the
Wikipedia article for sheaves does not make this quite as easy and
simple as the sketch above. But you can find the sheaf axioms there,
and with some head-scratching and effort, you can see its the same thing.
(Technically, the jigsaws result in a pre-sheaf, not a sheaf. The
difference is a 5th axiom, which (as as of 2018-2024) does not seem
relevant for AI and machine learning.

Why is this idea useful for AI?

Consider theorem proving (or any kind of 'reasoning': abductive, inductive,
whatever). Here, one typically has some "inference axioms", which are
used connect statements together to get the inference result.  For example,
“No man is an island" and "Aristotle is a man" -> “Aristotle is not an island"
makes clear that 'man' is the attachement (jigsaw connector). Hooking
together 'man' across the two statements allows the inference to be
made.

Another example arises in natural language parsing. A transitive verb
(V) takes a subject (S, a noun) and an object (O, another noun) and if
one provides the S and the O, then one gets a valid sentence. For
example, "to throw" is a transitive verb. "John threw the ball" has
“John” as subject, “ball” as object and "throw" as the verb, Each word,
on it’s own, is a jigsaw piece with unconnected connectors. (The 1991
Link Grammar papers show this explicitly.)

A vector embedding for the verb "to throw" is a list of all possible
subjects, and all possible objects. This list forms a vector, and each
item in the vector can be given an associated weight (or probability,
or log probability, or information content... etc.) You can throw a
ball (p=1.0), a cinder block (p=0.5), a book case (p=0.01), a car
(p=0.0001) and the rest aren’t throwable (clouds, oceans, mountains,
p=0.0) You’ll need a three-way information content if you want to talk
about meteors throwing mountains into the air. Context matters.

Vector embeddings of this type obey the old WordVec results of
"King - Man + Woman = Queen". More careful work shows that they
distribute onto a very high-dimensional Gaussian Orthogonal Ensemble
(GOE) and so cosine-distance works well for clustering & similarity.

Once you see jigsaw pieces and how they assemble, you can’t un-see them.

In lambda calculus, beta reduction is just jigsaw assembly.  Beta
reduction is just a formal description of function composition. For
example, in C/C++/Java, if you declare a function `int f(int x)` then
you can call it with `int x=42`, but not with `double pi=3.14159`. The
int/double is the type of the connector (a la ‘type theory’); you can
connect if the types match, else you cannot.

In tensor calculus, tensor index contraction is just jigsaw assembly.
In electronics, wiring together circuits is just jigsaw assembly.
In describing biochemical reaction cycles, the reactions are jigsaw
pieces. They are everywhere, and they are foundational.

What makes sheaves interesting for AI research is their dual nature:
they have both a syntactic aspect, of describing 'what connects to what'
and a vector-embedding context, allowing the theory of statistical
ensembles to be applied. Insofar as neural nets are statistical
ensembles, and grammar is symbolic AI, then ensembles of jigsaw pieces
sit smack-dab in the middle of neuro-symbolic AI.

Sheafs - Slightly more formal
=============================
Sheafs provide a simple and convenient mechanism for working with
graphs "locally", by making the nearest-neighbors of a vertex apparent.

The traditional textbook-canonical way of specifying a graph is to
state that it is a set of vertexes, and a set of edges that connect
pairs of vertexes.  The problem with this description is that given
any vertex, one has no idea of what edges are connected to it, without
scanning the entire set of edges. Another problem is that vertexes and
edges are not composable; that is, when they are composed together,
they are no longer vertexes or edges, but a more general type: a
"subgraph".  By contrast, sheaves carry local information, and are
composable.

Given a vertex V, a "section" is defined as a set of pairs (V,E) of
that vertex V and all edges E that are attached to it.  That's it!
Very simple!  A section can be envisioned as a "spider", with the
vertex V as the body of the spider, and the edges as the legs of the
spider.

Sections are composable, in that several can be connected together
by joining ("connecting") edges.  The result is still a section, in
that it has a central blob as the spider-body, and a whole bunch of
legs sticking out. Composing sections in such a way that the edges
connect only in legal ways is called "parsing".

Another way of visualizing sections is to envision a jigsaw-puzzle
piece instead of a spider. The vertex V is a label on the puzzle-piece,
and each leg is a tab or slot on the puzzle-piece. The tabs or slots
are now obviously connectors: this emphasizes that jigsaw-puzzle pieces
can be connected together legally only when the connectors fit together.
Again: the act of fitting together puzzle-pieces in a legal fashion is
termed "parsing".

In standard mathematical terminology, the spider-body or jigsaw-label
is called the "germ". It is meant to evoke the idea of a germinating
seed, as will become clear below.

Diagrammatic illustrations of jig-saw puzzle-pieces can be found here:

* Sleator, Temperley, [Parsing English with a Link Grammar](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/link/pub/www/papers/ps/tr91-196.pdf)
  See also [ArXiv cmp-lg/9508004](http://arxiv.org/pdf/cmp-lg/9508004).
* Bob Coeke, [New Scientist: Quantum Links Let Computers Read](http://www.cs.ox.ac.uk/people/bob.coecke/NewScientist.pdf)

A more formal technique for visualizing connected edges in a monoidal or
tensor category is the [string diagram](https://ncatlab.org/nlab/show/string+diagram).

Some of the mathematical foundations can be understood by noticing that
a [pregroup grammar](https://en.wikipedia.org/wiki/Pregroup_grammar) is
effectively the same thing as Link Grammar, where the left-right
relations such as S/VP and S\NP are replaced by type symbols
(type-theoretic types). The pregroup grammar also provides a good
working example of a monoidal category that is NOT left-right symmetric.
This is important, because the monoidal categories that describe tensors
and Hilbert spaces and "quantum mechanics" and "linear logic" are all
left-right symmetric. This is why some people think that language looks
like "quantum", and is also why they're right, but not quite.

The "sheaves" here are exactly the same ones as in "sheaf theory", that
branch of mathematics encountered in algebraic topology. Formally, the
"puzzle pieces" assemble in such a way that the axioms of a (pre)-sheaf
are obeyed.

The earliest reference to this collection of ideas (of linguistics as
type-theoretic algebraic objects with sheaf-like, monoidal properties)
appears in the book
[*"Algebraic Linguistics; Analytical Models"*](https://monoskop.org/images/2/26/Marcus_Solomon_editor_Algebraic_Linguistics_Analytical_Models_1967.pdf),
from 1967, by Solomon Marcus (published by Elsevier).


Sections in Atomese
-------------------
Sections are represented in Atomese as follows:
```
     Section
         Atom "foo" ; usually a Node of some kind.
         ConnectorSeq
             Connector
                 Atom "bar"      ; for example, a WordNode.
                 LabelAtom "foo-to-bar label" ; can be a PredicateNode.
             Connector
                ....
```
Here, `(Atom "foo")` is the spider-body or germ. Each leg of the spider
is represented by a `Connector` link. In the above example, the vertex
"foo" has an edge "foo-bar" attached to it, with `(Atom "bar")` being
the far-endpoint of that edge. The edge carries an optional label,
shown as `LabelAtom`, above.

The "foo-bar" edge can also be represented as an `EdgeLink`,
which is the structure used in many other parts of OpenCog.
```
       EdgeLink
           LabelAtom "foo-to-bar label" ; can be a PredicateNode.
           ListLink
               Atom "foo"
               Atom "bar"
```
This `EdgeLink`, and the `Section...Connector` structure are meant
to be sort-of, more-or-less equivalent and interchangeable. (In many
cases, they can be taken to be equivalent; however, the `Section...
Connector` structure is more general and can describe more kinds of
structures more simply than an `EdgeLink` can.  This will be made
clear below).

Connectors
----------
Note that `Connector`s are like "half-edges": in isolation, they carry
the edge-label, and the far endpoint, but not the near endpoint.  The
term "connector" is used to emphasize that these are meant to behave
like the tabs on a jigsaw-puzzle piece: that they serve to connect to
other connectors on other sections.

There are no explicit or implicit rules for how to connect up
connectors; this is application-dependent.  However, the above example
implicitly implies that a connection is legal only when the germ-atom
of one section is the same as a connector-atom in another section.
Likewise, the edge-labels should match up or be coherent in some way.

The `ConnectorSeq` link is an ordered link, rather than an unordered
set.  This is because in many applications, the sequential order of the
connectors matter.

Germs and Etales
----------------
In the above, the germ `(Atom "foo")` can be taken to be a label "foo"
on the vertex that is the germ of the section.  There is no requirement
that this label be unique, or that it uniquely identify a vertex.  Thus,
in general, there will be many different sections having the same germ.

In this case, it is common to visualize sections as pieces of paper,
stacked one on top another, aligned so that the germs are always above
one-another. This stacking is referred to as a "stalk". Alternately, the
stalk can be visualized as a fir-tree or pine-tree, where annual growths
define the trunk, and then branches shoot off to the side, on level
planes or "etales". A single section is then just that etale, that plane
of branches from that single year of growth.  Unlike a pine-tree, however,
sections, like jigsaw-puzzle pieces, can be assembled together to make
a bigger section. Such sub-assemblies are still "etales", in that they
are all at the same level.

Sheaf Theory
------------
The collection of all possible stalks, and the sections on them,
together with the rules that dictate how the connectors can connect
is terms a "sheaf".

The axioms of sheaf theory can be understood as saying that the
puzzle-pieces can only be assembled in certain legal ways, and that
sub-assemblies can be intersected with one-another, as long as they
are correctly lined up.  The Wikipedia page for Sheaf Theory bears
very little resemblance to the description above, even though they are
both taking about the same concepts.  That is because the typical
application of Sheaf Theory in mathematics deals with sections that
have an infinite number (countable and uncountable) of connectors, and
the connector labels form a ring or a field.  That is not the case here,
and so the situation is conceptually simpler, here.

In relation to Graph Theory
---------------------------
Note that the above started by talking about graphs, but ends at a
somewhat different place.  It is worth reviewing the differences.
In the canonical description of a graph, it is implicitly assumed,
without statement, that each vertex is unique and unambiguous and
obviously a different vertex than any of the others. Likewise for
edges: they join pairs of vertexes, and there is no confusion about
what is being connected.

This is not the case for sheaves: although a germ can be visualized as
a single vertex, the fact that there is a stalk above the germ implies
that many vertexes in the graph are taken to be "the same vertex". Yet,
despite being the "same vertex", each section connects to the graph in
different ways.  Thus, a sheaf can be visualized as a graph with a
certain quotient operation applied, a lumping together of certain
vertexes into a common set, the "germ".

In graph theory, an edge unambiguously connects two vertexes. By
contrast, the connectors on a section are a bit more ambiguous: they can
connect to anything else that is legally connectable: the connectors
must match, must be contractible.  The connectability of connectors
are given by rules, but those rules are "user-defined" (although they
usually match connectors to germs and force edge-label agreement).

In relation to Tensor Algebra
-----------------------------
A tensor can also be visualized as a kind-of spider or puzzle-piece: the
legs or tabs are just the indexes on the tensor, and the number of legs
is the degree of the tensor.  The act of contracting tensor indexes is
the same as connecting together connectors.

More properly, a tensor is a linear combination of such puzzle-pieces;
it is a linear combination of the etales of the stalk.  Thus, for
example, an N-dimensional vector in an N-dimensional space can be
visualized as a linear sum of N different connectors (that is, as a sum
of one-legged spiders). Each basis element of the vector space
corresponds to a single connector. When one takes a dot-product of
two vectors, one joins connector-to-connector, aligning the basis
elements of the vector, and sums up the values.

Thus, if one has a list of connectors, and a floating-point value
attached to each, this can be visualized as a vector. Examples arise
in linguistics, where one has lists of words, and the associated counts:
these form a vector.

In the more general case, a tensor of degree K is a linear sum of spiders
or puzzle-pieces with K legs/connectors on them.  A tensor is a "stalk"
with a number attached to each "etale" or section.  Tensors can be
contracted together to form other tensors simply by connecting some of
the legs/connectors.  Multiplication and co-multiplication in a tensor
algebra are just specific rules for connecting connectors so that
linearity is preserved.

The N-gram in linguistics an example of an degree-N tensor. The count of
the number of occurrences that an N-gram is seen is the weight.
Sometimes the collection of N-grams is called a "vector", but this is
not quite right, because when multiple N-grams are assembled to form a
sentence, the words must match-up correctly: N-grams are tensor
elements.

In relation to Pregroup Grammars and Category Theory
----------------------------------------------------
The contraction rules for the connectors imply that they form a kind of
pre-group grammar (see the Wikipedia article).  Such grammars are
examples of non-symmetric monoidal categories.

This viewpoint is rather complex, and is not elaborated here. However,
it is useful: it provides insight into the nature of parsing, and
explains the correspondence (Curry-Howard correspondence) between
categories and internal languages. That is, languages are parsed, and
the act of parsing is the discovery of those sections that have legal
joinings of connectors.

References:
* John Baez, [Physics, Topology, Logic and Computation: A Rosetta Stone](http://math.ucr.edu/home/baez/rosetta.pdf)
* Multiple publications from Bob Coecke.
* Jean-Yves Girard, "Locus Solum"

In relation to Link Grammar
---------------------------
The definition and use of sections and connectors here is directly
inspired by the theory of Link Grammar.  The intent of the machinery
here is to provide a generalized version of Link Grammar suitable for
the discovery and analysis of generalized networks of relationships
between concepts and events taken from observations of the external
world.

In link-grammar, "germs" are called "lexical items". The "connector
sequences" are called "disjuncts". The "stalk" is a lexical entry or
dictionary entry. The fact that the disjuncts are disjoined is a way of
saying that each etale (section) of a germ is distinct.

Terminology
===========
Some recurring terms are used in the code, and are defined here:

* "germ"      -- This is the spider-body: the vertex at the center.
* "section"   -- This is a single spider (jigsaw-piece), having a single
                 germ at it's center. (Note: this is in conflict with
                 common mathematical terminology, where a section consists
                 of one or more connected spiders/jigsaw-pieces).
* "connector" -- As described above; an edge attached to a germ.
* "stalk"     -- This is the collection of all sections that have the
                 same germ. The intent is that a stalk can be pictured
                 as the main stem or stalk of a plant. The connectors
                 are then like branches on a fir-tree (i.e. are all at
                 the same level or section).


Network Inference and Analysis Tools
====================================
In this project, there's a generic theme of inferring structure from
a sequence of events.  That is, a sequence of events is observed in the
external world, and the question arises: are these events correlated?
Do they mean something when observed together, or is it happenstance?
What is the relationship between the items in the sequence of events?

The presumption made here is that the sequence of observed events are
being generated by multiple different actors, who are performing actions
independently of one-another, and sometimes exchanging messages with
one-another, along a network.  In the simplest case, the observed events
are the sequence of actions performed by each of the actors, placed in
some time-like sequential order.  The goal of the code in this directory
is to statistically infer the structure of the network, given a sequence
of observed events.

Theoretical computer science has explored a number of theories for
describing the relationships between ordered events, and their
interpretation as a network; these theories are inter-related, and
go under the name of:

 * Sequent Calculus (proof theory)
 * Process Calculus
 * Calculus of Communicating Systems (CCS)
 * The theory of Communicating Sequential Processes (CSP)
 * History monoids, trace monoids and Trace theory
 * Actor model
 * Dependency grammar
 * Sheaf theory

See the respective Wikipedia articles on each of these topics. A
lightning review of these concepts is given below.


Networks and Sheaf theory
-------------------------
The topological structure of a graph can be understood locally in terms
of "sheaf theory". In this framework, instead of looking at a graph as
whole, one instead looks at it locally, in terms of how any given vertex
attaches to the other vertexes around it.

Thus, isolating a single vertex in the graph, one can see that it has N
different edges attaching it to other vertexes in the graph: one says
that the degree of the vertex is N.  The "section" of the vertex is the
list of the N edges that are attached to it.  The graph, as a whole,
is then described by listing each vertex in it, and the section
associated with it.

This should be contrasted to the traditional, canonical description of
a graph, as a list all of the vertexes and all of the edges. The problem
with the canonical description is that the local connectivity of the
graph is hidden: one has to traverse the entire list of edges to find
those that are connected to a particular vertex. This is inconvenient
and inefficient.

Thus, a typical section might look like this:
```
    Section
        LexicalAtom "something"
        ConnectorSeq
            Connector
                LexicalAtom "it's"
                ConnectorDir "-"
            Connector
                LexicalAtom "curious"
                ConnectorDir "+"
```

The above encodes the idea that the vertex "something" has an edge that
connects it to the vertex "it's", and another edge that connects it to
the vertex "curious".

The `Section`, `ConnectorSeq`, `Connector` and `ConnectorDir` are real
atom types.  The `LexicalAtom` is not: its just an example. The word
"lexical" is used here to suggest that the above has the form of a
dictionary entry: one can look up "something" in the dictionary, and,
obtain as it's definition, the `ConnectorSeq` of everything it attaches
to.

The `ConnectorDir` will be explained later. In general, one may want to
include additional information about a connector: a weight, a distance,
its commutativity properties, etc.


Stalks, Germs, Lexical Items
----------------------------
A base presumption here is that a given vertex participates not in just
one network graph, but in millions of them. A single network can then be
viewed as a single global section of sheaf; it is the abstract collection
of all of the networks that comprises the sheaf.

By making a large number of statistical observations of graphs, and
then collecting statistics on sections, one can hope to discern how a
vertex typically connects into a typical graph. In sheaf theory, this
information about the typical behavior of a vertex is called the "stalk"
of the vertex, the vertex being the "germ" from which the stalk grows.

The "gluing axioms" of a sheaf can be thought as describing how different
connectors can be joined together.  The act of parsing requires selecting
a single disjunct out of the disjoint union of all of them; thus, the
disjoint union is a set of 'possibilities' or parts of a 'possible
world', and so can be understood in terms of Kripke-Joyal semantics.


Grammar
-------
In linguistics and in computer science, the stalk/germ can be viewed as
a grammatical entry in a grammar.  A grammar describes how a "word" in
a "language" can be related to other words in the language.

In the theory of language, one is presented with a linear sequence of
words, a "sentence".  There are two primary styles for expressing
grammars: production grammars and dependency grammars. Production
grammars ("constituency grammars", "head-phrase structure grammars"
(HPSG) and so on) are expressed in terms of re-write or production rules.
These kinds of grammars are often classified according to the Chomsky
hierarchy: regular grammars that describe regular expressions (finite
state machines), context-free grammars that describe push-down automata,
etc.

A dependency grammar describes links or dependencies between the words
in a sentence. The prototypical dependency grammar is that of Tesnière.
Link Grammar is an example of a dependency grammar.  Note that, for
every production grammar, there is an equivalent dependency grammar,
and vice versa.  The lexis of a dependency grammar can be written as a
collection of sections; this is made very explicit in Link Grammar.

A language is then the same thing as the "étalé space" of a sheaf.


Trace theory
------------
In a language, sentences are linear sequences of words. The constraint
of linear. ordered sequences is loosened in trace theory, CSP and CCS.
Those theories describe "partially-commutative monoids", where the
concept of a sentence is replaced by the concept of a "trace". A trace
is a sequence, where the order of some of the items in the sequence is
not important.

An example from natural language might be:
```
   "This (maybe, is) an example."
```
which encodes the idea that the two sentences: "This maybe is an
example." is "This is maybe an example." have more or less the same
meaning, and that the word-order for `(maybe, is)` essentially did not
matter.

Relative order dependence/independence typically occurs in any situation
where there are instructions for performing actions.  Thus, "place
flour, salt and water in a bowl" does not specify what order these
ingredients are placed in the bowl; what does matter is that this is
performed before placing the mixture in the oven.  In computer science,
this describes the notion of parallel processing: some computations can
be done in parallel, as long as they are all completed before some other
computation is performed.  Serialization ("rendezvous") primitives are
mutexes and semaphores.

In trace theory, one has the general idea that one is observing traces
as the result of computations being performed by distinct agents or
actors exchanging messages between one-another; the ordering of some
messages does not matter; the order of others do.  The sequence of
observed messages is the "trace".


Trace and History Monoids
-------------------------
The example
```
   "This (maybe, is) an example."
```
is an example of a "trace monoid" (see Wikipedia).  For every trace
monoid, there is an equivalent (isomorphic) "history monoid". For this
example, it is
```
  [This, This] [maybe, .] [., is] [an, an] [example, example]
```
This makes clear that the network consists of two actors, both of
which move to the state "This", initially. Then one actor moves to
the state "maybe", while the other simultaneously moves to the state
"is". Both then move to the state "an", followed by "example".  The two
actors presumably exchange messages to accomplish this synchronization;
those messages are not visible in the trace; only the sequence of states
are.


Cause and effect
----------------
Whenever a sequence of events is observed for a system, the apparent
order of "cause" and "effect" can be reversed, with the apparent "cause"
coming long after the "effect".

For example: to build a high-rise building, a foundation must be dug
first.  Observed as events in time, the construction comes after the
foundation is built. However, it would be incorrect to say that a hole
in the ground "causes" a sky-scraper to appear, even though it came
earlier.  This is because the formal cause of the skyscraper is the
will of a real-estate developer; yet, this will is not observed; only
the construction events are.  From the point of view of a dependency
grammar describing the sequence of events, the skyscraper should be
viewed as the "head", and the hole in the ground as the "dependent",
with the head dominating the dependent, or "causing" the dependent,
even though the dependent comes earlier.

This example demonstrates why Hidden Markov Model (HMM) and Bayesian
network models of human language fail: The earlier words in a sentence
do not, cannot "cause" later words to appear; rather, it is often the
case that the later words "cause" or "force" the earlier words to appear.

More generally, this example shows why a dependency grammar approach,
with events associated with "sites", "germs", "stalks", "lexical
entries" is more appropriate for the analysis of a network, and is more
powerful, than HMM, Bayesian networks or Latent Semantic Analysis (LSA)
can be.

Random jab: this is also why Hutter's naive AIXI is incorrect: it only
considers past events, thus incorrectly inferring in a forward time-like
direction.  Clearly, this leads to incorrect conclusions about
skyscrapers, and fails to induce the Aristotelian "formal cause" of
events.


Inferring Grammar
----------------
To observationally infer the grammar of a network, one must observe a
lot of networks. With each network observation, one may create a set
of sections summarizing that network.  To get a view of the general
network, counts can be maintained for each observed section.  The result
of such counting is a frequency distribution over sections. To induce a
grammar, one may then compare the distributions on different vertexes;
if they are sufficiently similar, the vertexes can be grouped together
into a class. Since vertexes also occur as the end-points of connectors,
a grouping also has to be observed there.

If the network is not apparent (if it is latent or hidden), then one
cannot directly generate sections, because one cannot directly observe
the edges. In this case, a more round-about route is required, as
follows:

1) Assume all possible networks occur with equal probability.
2) Observe a lot of sequences, and count the frequency with which edges
   occur.
3) Compute the mutual information (MI) for each edge. That is, each edge
   has two endpoints, and the co-occurrence of these endpoints can be
   captured as the mutual information between them.  The MI serves as
   a kind of measure of covariance or correlation.
4) Re-observe a lot of sequences, this time over-laying them with a
   maximal spanning tree (MST). So unlike step 1, where each network was
   assumed to be a clique, this time, the network is assumed to be a
   tree.  The "correct" tree is the tree that maximizes the sum of the
   MI of the edges.
5) Compute the sections of the MST, and accumulate these to obtain a
   distribution of sections.

The grammar can then be inferred from the distribution of the sections.


MST parsing
-----------
The MST parser is a "Maximum Spanning Tree" parser. Given a specific
ordered sequence of events, viz a sequence of atoms, and given a large
pool of observed dependencies between pairs of events, the MST parser
will construct a spanning, planar dependency tree such that:

* The tree is a spanning tree: all nodes are connected.
* The tree is planar (projective): it is flat, with no crossing edges.
* The sum-total score of the weights on the edges is maximized.

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

The MST parse just creates a planar (projective) tree connecting all
of the atoms in a sequence, such that the sum-total (addition) of the
scores of all the edges in the tree is maximal, as compared to any
other tree.

After the MST parse, the section for each vertex in the parse can be
computed.

MPG parsing
-----------
The MPG parser is the "Maximal Planar Graph" parser; it starts with
the parse tree from an MST parse, and then adds edges, one at a time,
of the highest score, such that the graph remains planar (no
intersecting edges.) The result is a graph with the largest possible
number of edges, such that it is still flat, and such that the edges
are scored the highest.
