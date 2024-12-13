#! /usr/bin/env python3
#
# storage_tutorial.py
#
"""
A basic tutorial showing some of the basic concepts from the OpenCog
AtomSpace.
"""

from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.storage import *
from opencog.storage_rocks import *

space = AtomSpace()
set_default_atomspace(space)

# Record a photograph stored in a directory.
#
# This has the form of a labelled directed graph edge.
# In ASCII graphics:
#
#                      "some edge label"
#    "from vertex" ------------------------> "to vertex"
#
# which in Atomese, becomes
#
#    (Edge (Predicate "some edge label")
#            (List (Item "from vertex") (Item "to vertex")))
#
# and for python, the parens get re-arranged and commas are inserted:
#
#    Edge (Predicate ("some edge label"),
#          List (Item ("from vertex"), Item ("to vertex")))
#
# Photographs are "stored" (or can be found at) URL locations.
# The relationship between the URL location and the file name can
# be indicated with an arrow. (This is one of many ways.)
e = EdgeLink(
	# Here, "URL" is just some string. Any string will do.
	PredicateNode("URL"),
	ListLink(
		# The name of the directory with photos in it.
		ItemNode("file:///Home Computer/folders/My photo album"),

		# The photo itself.
		ItemNode("Fantastic Sunset on Sunday.jpg")))

print("Here's your data:", e)

# -------------------------------------------

# AtomSpace contents can be loaded, stored and send over the net.
# Below, a StorageNode is used save selected AtomSpace contents
# to a RocksDB database. Rocks is nice, because it requires no
# config to use.

# Create a RocksDB StorageNode, and open a connection to it.
# The rocks:// URL specifies a directory in the local filesystem.
storage = RocksStorageNode("rocks:///tmp/foo")
cog_open(storage)

# Store the one and only edge created above.
store_atom(e)

# Close the connection to storage.
cog_close(storage)
print("Closed the connection to storage")

# The rest of this demo is about restoring the Atom that was just saved.
# To prove that this works, the AtomSpace will be cleared. To prove that
# it was cleared, look at the contents before and after.

# Define a utility printer
def prt_atomspace_contents(asp) :
	print("AtomSpace contains a total of " + str(len(space)) + " Atoms")
	if 0 < len(asp) :
		print("These are:")
	count = 0
	for atom in asp:
		count += 1
		print("Atom " + str(count) + ".... " + str(atom))

print("The AtomSpace before clearing:")
prt_atomspace_contents(space)

print("\nWill now clear the AtomSpace.")
space.clear()
print("The AtomSpace size after clearing: ", len(space))
prt_atomspace_contents(space)

# The clear clobbers the StorageNode; create it again.
storage = RocksStorageNode("rocks:///tmp/foo")
cog_open(storage)

print("Restore one atom: the file, whose name we magically know already.")
fetch_atom(PredicateNode("URL"))
prt_atomspace_contents(space)

print("Restore all Edges in storage.")
fetch_incoming_set(PredicateNode("URL"))

# Close the connection to storage.
cog_close(storage)

print("After restoring, the AtomSpace size is " + str(len(space)))
prt_atomspace_contents(space)

print("Good bye!")
