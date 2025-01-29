#! /usr/bin/env python3
#
# vector_tutorial.py -- Example of converting query results to vectors.
#
# The AtomSpace provides a powerful graph query engine that can extract
# subgraphs of arbitrary shape. Such query results are presented as
# rows, whereas many common python platforms prefer to work with
# columns. This example shows how to perform a basic query, and convert
# the results to columns.
#
# ------------------------------------------------------------------
# Python setup.

from opencog.atomspace import AtomSpace, types
from opencog.execute import execute_atom
from opencog.type_constructors import *

atomspace = AtomSpace()
set_default_atomspace(atomspace)

# ------------------------------------------------------------------
# Start by populating the AtomSpace with some data.
# For this example, the data will be very regular, for readability.
# (Thus, it could also be easily taken from some SQL database; the
# AtomSpace is designed for very irregular graphs, so the uniformity
# here hides what the AtomSpace is actually good for. No matter.
# Moving on ...)

tag = PredicateNode("word-pair")

# A standard dependency parse, showing word-pairs.
EdgeLink(tag, ListLink(ItemNode("the"), ItemNode("dog")))
EdgeLink(tag, ListLink(ItemNode("dog"), ItemNode("chased")))
EdgeLink(tag, ListLink(ItemNode("the"), ItemNode("cat")))
EdgeLink(tag, ListLink(ItemNode("chased"), ItemNode("cat")))
EdgeLink(tag, ListLink(ItemNode("chased"), ItemNode("around")))
EdgeLink(tag, ListLink(ItemNode("the"), ItemNode("house")))
EdgeLink(tag, ListLink(ItemNode("around"), ItemNode("house")))
EdgeLink(tag, ListLink(ItemNode("cat"), ItemNode("around")))
EdgeLink(tag, ListLink(ItemNode("HEAD"), ItemNode("dog")))
EdgeLink(tag, ListLink(ItemNode("HEAD"), ItemNode("chased")))

# A pattern that will fid tagged word-pairs
pair_pattern = EdgeLink(tag,
    ListLink(VariableNode("$left-word"), VariableNode("$right-word")))

# A query pattern, with typed variables
basic_query = QueryLink(
    VariableList(
        TypedVariableLink(
            VariableNode("$left-word"), TypeNode("ItemNode")),
        TypedVariableLink(
            VariableNode("$right-word"), TypeNode("ItemNode"))),

    # Search for pairs that are present in the AtomSpace
    PresentLink(pair_pattern),

    # Output what was found
    pair_pattern)

# Perform the query.
execute_atom(atomspace, basic_query)

print("Basic query returned:",
    execute_atom(atomspace, ValueOfLink(basic_query, basic_query)))
