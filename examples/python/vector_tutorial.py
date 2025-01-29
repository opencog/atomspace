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

