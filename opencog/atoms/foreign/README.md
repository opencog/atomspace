
Foreign Abstract Syntax Trees
=============================
The source code of almost any programming language can be converted into an
"[abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)".
The AtomSpace stores trees. Therefore, in principle, almost any system
can be mapped onto the AtomSpace. All that is missing is the pretty-printing:
the ability to ingest those trees into the AtomSpace, and then print them
back out, in proper form.

Examples of systems with obvious tree like-structures include JSON, YAML,
datalog, s-expressions and XML.  Of course, you could map any langauge
(including python and java) into trees; just that the above are
conventionally used to hold some kind of data. The primary reason you
would want to map such data-holding languages into the AtomSpace would
be to make use of the query engine, to perform data searches on them.

The code here is an early-stage experiment to perform that mapping. It
is currently at the proof-of-concept stage. The demos work.

The primary unresolved issue is whether this is a worthwhile project.
The mappings are doable; they're just not that hard. Clearly, the next
step would be to map the corresponding query language, such as GraphQL
or the grakn.ai DataDB for the JSON subset, or SparQL for the XML
subset.  Mapping these onto the AtomSpace query engine should not be
that hard: the query engine provides a superset of the tools that these
other systems provide. It might even be faster! The primary issue is
that its a bit ... boring.  Unless someone has a particular use-case
in mind, there's not much of a point to this, right?
