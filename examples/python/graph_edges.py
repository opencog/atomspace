#! /usr/bin/env python3
#
# graph_edges.py
#
"""
Representing graph edges in basic Atomese. This demo constructs a
single graph edge, using the canonical (i.e. "recommended") Atomese
style for this. It is not the only way; there are no particularly
strong rules for how Atomese "must" be used, and thus graph edges,
and most things can be represented in a variety of ways. As they say,
"limited by your imagination". However, the canonical form is obvious,
which is why it is recommended.
"""

# Boilerplate. Import what we need.
from opencog.atomspace import AtomSpace
from opencog.type_constructors import *

# Specify an AtomSpace to work with.
space = AtomSpace()
set_thread_atomspace(space)

# Create a labelled directed graph edge. In ASCII graphics:
#
#                      "some edge label"
#    "from vertex" ------------------------> "to vertex"
#
# which in (scheme-style) Atomese, becomes
#
#    (Edge (Predicate "some edge label")
#            (List (Item "from vertex") (Item "to vertex")))
#
# In Python, the parens get re-arranged and commas are inserted:
#
#    Edge (Predicate ("some edge label"),
#          List (Item ("from vertex"), Item ("to vertex")))
#
e = EdgeLink(
	# Any UTF-8 string can be used as an edge label.
	PredicateNode("My URL collection"),
	ListLink(
		ItemNode("file:///Home Computer/folders/My photo album"),
		ItemNode("Fantastic Sunset on Sunday.jpg")))

print("Here's your data:\n", e)

# The "Link" and "Node" suffixes are optional.
f = Edge(
	Predicate("My URL collection"),
	List(
		Item("file:///usr/bin/"),
		Item("bash")))

print("Here's a tool:\n", f)

# THE END. That's All, Folks!
# -------------------------------------------
