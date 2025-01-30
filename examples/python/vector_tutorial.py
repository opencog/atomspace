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

# In case you are wondering what that parse looks like "in read life",
# here it is:
#
#     +--------->WV-------->+--------MVp--------+
#     +---->Wd-----+        +-----Os-----+      +----Js----+
#     |      +Ds**c+--Ss*s--+      +Ds**c+--Mp--+    +Ds**c+
#     |      |     |        |      |     |      |    |     |
# LEFT-WALL the  dog.n chased.v-d the  cat.n around the house.n
#
# The above table just collapses each of the link types to a single
# PredicateNode("word-pair") The extra complexity would just make
# this demo more confusing. ('S' is subject, 'O' is object, 'D' is
# determiner. 'M' is a prepositional modifier, and 'J' is object of
# the preposition.

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
# Design a more interesting query. This one will count the number of
# times that words appear on the left or right side of a pair. The
# resulting counts will be stored with the words (for later access).

# Define a key where the counts will be placed. The key can be any
# Atom at all, but by convetion, PredicateNodes are used. The naming
# convention was inspired by first-order predicate logic.
count_key = PredicateNode("counter")

# Define the word-counting query. The variable declarations and search
# pattern are same as before. The rewrite side of the query has two
# parts, incrementing different counts on a very short vector.
#
# The IncrementValueLink Atom is a thread-safe, atomic increment. It can
# be run from multiple threads at once, and will not race in reading,
# incremementing and writing the count.
#
# Two counts are kept: one for the left-words, and one for the
# right-words. These two counts could be kept under separate keys.
# However, to make the demo more interesting, these two counts are
# stored under just one key, but using a FloatValue vector to store
# them. Thus, the increment is performed not a single number, but on
# the whole vector, at once. The counts on the left-words are
# incremented with [1,0], and those on the right with [0,1]. Since
# increment is atomic, the counts will be accurate.
#
# In general, the increment vector can be of any length, holding
# arbitrary floating-point values. Try it.
counting_query = QueryLink(
    pair_vardecls,
    PresentLink(pair_pattern),

    # When the above pattern is matched, the counts are incremented.
    IncrementValueLink(VariableNode("$left-word"),
       count_key, NumberNode([1,0])),
    IncrementValueLink(VariableNode("$right-word"),
       count_key, NumberNode([0,1])))

# Perform the counting.
counting_query.execute()

# ------------------------------------------------------------------
# To view the count results, we'll have a bit more fun. Construct a
# query that crawls over words, only. Just for grins, a MeetLink is
# used, instead of a QueryLink. The MeetLink does NOT perform any
# rewrites: it just returns the goundings of the variable(s), directly.
# The name "Meet" come from the fact that this query peroforms a meet
# on a poset. See Wikipedia:
#    https://en.wikipedia.org/wiki/Join_and_meet
# Yes, Atomese also has a JoinLink.
#
get_words = MeetLink (
    TypedVariableLink( VariableNode("$word"), TypeNode("ItemNode")),
    PresentLink(VariableNode("$word")))

print("The set of words is:", get_words.execute())

# The above will print a list of all of the words found by that query.
# Notice that these are wrapped with a UniSetValue. The UniSet is a
# thread-safe producer-consumer deduplicating set. That means that one
# or more threads can write to it, while other threads remove content
# from it.  In this case, the query is done in an eyeblink, so there is
# no point in dealing with the complexity of multiple threads.
#
# The name UniSet comes from the fact that each entry in the set is
# unique: adding the same Atom a second time does nothing. The set
# contents are de-duplicated.
#
# There is also a QueueValue. This is a thread-safe producer-consumer
# FIFO. Writers add to the tail, readers rea from the head. Both the
# UniSet and the Queue block when empty, so that reader threads wait
# until something shows up for them to work on.

# ------------------------------------------------------------------
# Values are C++ vectors. Thus, FloatValue is just std::vector<double>
# and StringValue is just std::vector<std::string>. LinkValue's are
# vectors of pointers to Values or Atoms. These convert naturally to
# python lists.

# Get the cached UniSet from the earlier query:
the_unique_set_object = ValueOfLink(get_words, get_words).execute()

# Unwrap it, into a python list of Atoms
list_of_atoms = list(the_unique_set_object)
print("The list of word-atoms is", list_of_atoms)

# And a python list of strings:
list_of_word_strings = map(lambda wrd: wrd.name, list_of_atoms)

# ------------------------------------------------------------------
# Anyway ... moving on. We want to see the counts on the words.
# A purely pythonic way of vewing these:
for wrd in list_of_atoms:
	print(f"The word \'{wrd.name}\' has count {wrd.get_value(count_key) }")
