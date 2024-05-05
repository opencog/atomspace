;
; knowledge.scm - Representing data with Predicates and Evaluations
;
; The AtomSpace can be thought of as a database for (hyper-)graphs.
; The reason for using graphs, and, more specifically, hypergraphs,
; is to gain flexibility and freedom that is absent in conventional
; databases.
;
; Thus, a quick sketch and critique of other database styles is in
; order. Starting the SQL: one issue is that SQL tables need to be
; defined, before putting data in. The tables have a fixed number of
; columns, so that if you discover later that you need more columns,
; you have to start all over. If you have more than a few hundreds
; different kinds of tables, you're in trouble.
;
; You can avoid the fixed-table problem by using JSON or YAML, but
; then you give up searchability. You can make up for that by using
; GraphQL or maybe SparQL, but then you fall back into the *QL query
; style. The AtomSpace query system is more powerful than the *QL style;
; the next few demos will make this clear. A formal treament can be
; found in the PDF "Graphs, Metagraphs, RAM, CPU" in this repo, at
; opencog/sheaf/docs/ram-cpu.pdf or alternately, online:
;   https://github.com/opencog/atomspace/raw/master/opencog/sheaf/docs/ram-cpu.pdf
;
; The so-called "nosql" databases, or key-value databases avoid some of
; the hassle of SQL, but have the downside of making complex structures
; hard to represent. The typical graph databases, such as Neo4J, allow
; complex structures, but lack the tools for simple & fast query,
; forcing the user to walk from one vertex to another, following edges.
;
; A different rebellion against the strictures of SQL were the so-called
; "triple stores". This is exactly what is needed to store an arbitrary
; graph. It falters under the weight of being two low-level. What if you
; want to store four things, instead of three? You can; it can be broken
; up into triples, but then things get verbose and awkward.
;
; The goal of the AtomSpace is to offer freedom and flexibility that
; runs far beyond what can ordinarily be found in graph databases.
;
; It is assumed that the reader is familiar with the basic concepts
; of knowlede representation. This includes ideas such as "semantic
; network", "frame", "rule", "RDF", "axiom", "schema", "predicate"
; "term algebra", "term rewriting", "inference", "atomic sentence". The
; reader is strongly encouraged to review Wikipedia articles on these
; topics, as necessary, to acquaint themselves with why the AtomSpace
; works the way it does. The goal of the AtomSpace is to make working
; with any (and every) of the above concepts simple and easy.
;
; Never-the-less, some basic terminology:
;
; A "graph" is a collection of vertices and edges; each edge is a pair
; of vertices, thus asserting a relationship between them. Such a
; relationship can be thought of as a "predicate", which is true, if the
; edge exists, and is otherwise false.
;
; A hypergraph replaces the concept of an edge with a "Link", which can
; contain zero, one, two, three, or more vertices; roughly speaking, a
; link is a set. A further generalization allows links to contain other
; links, as well as vertices. This is much as sets can contain other
; sets. To distinguish from ordinary graphs, verteces are called "Nodes"
; in the AtomSpace. Something which can be either a node or a link is
; called an Atom. Note the capitalization convention.
;
; Recall some of the basic concepts of a "term algebra": a "term" may be
; a constant or a variable, or a functional grouping of constants and
; variables, such as P(a,b,c,x,y,z). Note that both constants and
; variables look like (labelled) vertices, thus Nodes, while a function
; looks like a (named) set, thus a Link. Relations look similar, except
; that relations are given a true/false value, so that a=b is true or
; false if a is or is not equal to be. Equivalently, equality can be
; thought of as a graph edge, with a vertex "a", another vertex "b",
; and the equals sign being an edge carrying the label "=". This label
; is a predicate: it is true or it is false.
;
; Effectively all concepts from logic, including axioms and schemas
; and inference rules, can also be mapped to graphs in a relatively
; straightforward fashion. It is even easier to represent them as
; hypergraphs.  There is a huge freedom in doing this, and it is up
; to you, the user, as to how to do that mapping. The AtomSpace does
; come with a large variety of conventional mappings, but none of these
; are manditory; you can do them in other ways.
;
; The below takes a simple example, taken from natural language
; processing.
;

(use-modules (opencog))

; Some typical natural-language dependency data.  Consider a dependency
; parse of the sentence "Susan makes pottery". Here, "makes" is the
; verb, and "pottery" is the object of the verb. Thus, the dependency
; is "_obj(make, pottery)". This can be represented as
;
(Evaluation
	(Predicate "_obj")
	(ListLink
		(Concept "make")
		(Concept "pottery")))

; There is no need to write this on five lines, nor to indent:
; you can write it on just one:
;
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "pottery")))

; "Evaluation" is the same type as "EvaluationLink",
; "Predicate" is the same type as "PredicateNode",
; "Concept" is the same type as "ConceptNode".
; Thus you can also write:

(EvaluationLink
	(PredicateNode "_obj")
	(ListLink
		(ConceptNode "make")
		(ConceptNode "pottery")))

; Nodes always have string names.
; Links are always collections of Nodes or of other Links.

; The above may seem verbose, and surely, you might think, there are
; freindlier and more compact ways to encode the sentence "Susan
; makes pottery". But this misses the point: the AtomSpace is designed
; to hold millions if not billions of expressions. The intent is that
; automated tools add and manipulate the Atoms in the AtomSpace, at
; the rate of tens of thousands per second. The above is not
; particularly "human readable"; the AtomSpace was designed to be
; "machine readable". The goal is to make it very easy and very fast
; for algorithms to read, write and alter the AtomSpace. So, although
; these demos show how to create Atomese, and explain what it means,
; the Atomese language itself is more like assembly code, or perhaps,
; more like a compiler intermediate language: for example:
;    https://en.wikipedia.org/wiki/Common_Intermediate_Language
; We think that Atomese is a *lot* more human readable than CIL !!
;
; --------------------------------------------------------
; Schemas.
;
; The above example is "free-form".  There are no particular rules for
; how you can put data into the knowledgebase. This makes it quite
; unlike SQL, where you MUST declare a table, before putting data into
; it. Thus, Atomese is a bit more like ProLog: you can assert facts,
; without specifying any kind of template or schema in advance.
;
; However, some kinds of data problems do want a strict structuring
; of data. This can be achieved by using the TypedAtomLink. Using types
; in the AtomSpace is an advanced topic, but some flavor of this can
; be given here.
;
; Suppose you wanted to say that the "_obj" predicate must always take
; only two arguments, no more and no less, and that these must always
; be ConceptNodes. You can declare this by writing the following:

(Signature
	(Evaluation
		(Predicate "_obj")
		(ListLink
			(Type "ConceptNode")
			(Type "ConceptNode"))))

; The above declares a "signature" with the standard comp-sci meaning:
;     https://en.wikipedia.org/wiki/Type_signature
; aka the standard meaning in mathematical logic:
;     https://en.wikipedia.org/wiki/Signature_(logic)
;
; To use it, it is useful to attach a name to it, so that it can be
; referenced by name:

(TypedAtom
	(DefinedType "my obj dependency relation")
	(Signature
		(Evaluation
			(Predicate "_obj")
			(ListLink
				(Type "ConceptNode")
				(Type "ConceptNode")))))

; The above can be used during type-checking, to verify that certain
; data structures are in the expected format. See
;     https://wiki.opencog.org/w/SignatureLink
; and
;     https://wiki.opencog.org/w/TypedAtomLink
; for more info. Again -- signatures are an advanced topic; you can
; ignore this for now. Just know that they exist, and are sometimes
; actually useful.
