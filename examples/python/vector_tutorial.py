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

set_default_atomspace(AtomSpace())

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

# ------------------------------------------------------------------
# Design a basic query pattern that can find the data above.
# The pattern just defines the "shape" of the data. The query defines
# the search itself, and what is to be done with the search results.
#
# A pattern that will find tagged word-pairs
pair_pattern = EdgeLink(tag,
    ListLink(VariableNode("$left-word"), VariableNode("$right-word")))

# A variable declaration for the pattern above. This is not strictly
# required, but is convenient to limit the scope of a query to variables
# of a given type. The types here are simple -- just ItemNodes. In
# general, typec can be arbitrarily complicated.
pair_vardecls = VariableList(
    TypedVariableLink(
        VariableNode("$left-word"), TypeNode("ItemNode")),
    TypedVariableLink(
        VariableNode("$right-word"), TypeNode("ItemNode")))

# Define a search for the above. It consists of the pattern, plus
# variable declarations for the (free) variables in the pattern
# (thus binding the variables.) The query is in the form of a rewrite
# rule: after matching the pattern, the variables can be used to
# define/create some new structure. For the demo below, a trivial
# rewrite is done: the initial search pattern is just echoed back.
basic_query = QueryLink(
    # The variable declarations that were constructed earlier.
    pair_vardecls,

    # The PresentLink asks that the serch pattern is present in
    # the AtomSpace. One or more search terms can be combined,
    # inluding a spec to insist that some term be *absent*, for
    # the query to match.
    PresentLink(pair_pattern),

    # Output what was found. This just repeats the pattern.
    pair_pattern)

# Perform the actual query. This is where the CPU time gets soaked up.
# For this simple demo, just milliseconds. For large datasets, maybe
# 25K to 150K queries per second (single-threaded), depending on the
# complexity of the search pattern and the actual dataset.
basic_query.execute()

# Verify the query results by printing them out. The query results are
# cached at a location given by using the query itself as the key. The
# cached results can be accessed at any time. Rerunning the query will
# update the cache.
#
# The ValueOfLink(atom, key), when executed, will return the Value
# on `atom` located at `key`. Below, the basic_query is used as it's
# own key: it's the "well-known location" that can always be found.
print("Basic query returned:",
    ValueOfLink(basic_query, basic_query).execute())

# ------------------------------------------------------------------
# Design a more interesting query that will count the number of times
# that words appear on the left, or the right side of a pair. The counts
# will be stored with the words.

# Define a key where the counts will be placed. The key can be any
# Atom at all, but by convetion, PredicateNodes are used. The naming
# convention was inspired by first-order predicate logic.
count_key = PredicateNode("counter")

counting_query = QueryLink(
    pair_vardecls,
    PresentLink(pair_pattern),
    IncrementValueLink(VariableNode("$left-word"),
       count_key, NumberNode([1,0])),
    IncrementValueLink(VariableNode("$right-word"),
       count_key, NumberNode([0,1])))

counting_query.execute()

print("Counting query returned:",
    ValueOfLink(counting_query, counting_query).execute())

