#! /usr/bin/env python3
#
# storage_tutorial.py
#
"""
A basic tutorial showing some of the basic concepts from the OpenCog
AtomSpace. This illustrates the basic concept of vertexes and edges;
it also illustrates how to save and restore to disk.

To run this demo, two other components need to be installed:
   https://gitub.com/opencog/atomspace-storage
and
   https://gitub.com/opencog/atomspace-rocks

The build and install steps are identical to those for the AtomSpace.
The `atomspace-storage` component provides a generic network and file
system API, while `atomspace-rocks` adapts it for the RocksDB database.

At the bottom of this file is some long commentary about how storage
interacts with the AtomSpace.
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
#
# The AtomSpace is an in-RAM database. You just might want to sometimes
# write some of it out to disk, and save it for later; or maybe you want
# to share AtomSpace contents with other AtomSpaces. This is accomplished
# with "storage" plug-in modules. These provide `StorageNodes` that can
# be opened, closed, read from and written to. As of this writing, there
# are four stable, supported modules for doing this:
#
# storage_rocks   - Stores the AtomSpace to disk, using RocksDB.
#                   This is the easiest to use: no configuration is
#                   required. Just start it and go. Also: its fast!
# storage_sql     - Stores the AtomSpace to a PostgreSQL database.
#                   This can be accessed/shared by multiple users,
#                   thus allowing a form of distributed processing.
#                   Three problems:
#                    * Postgres can be hard to configure for beginners
#                    * It's slower than the RocksDB node
#                    * The current implementation is stale and needs
#                      a refresh.
# storage_cog     - Communicates with another AtomSpace via TCP/IP.
#                   This can be accessed/shared by multiple users,
#                   thus allowing a form of distributed processing.
#                   This requires configuring a CogServer to run on
#                   a second network node, and so is not quite as
#                   easy as using RocksDB.
# storage_file    - Load and store AtomSpace contents as scheme
#                   s-expressions. This is 10x faster than using the
#                   scheme interpreter to load Atomese data. However,
#                   this is just a flat file: once written out, there
#                   is no way to automatically edit or update it.
#                   (It's just ASCII text, you can text-edit it, of
#                   course.)
#
# All of the above use exactly the same API, the `StorageNode` API.
# This demo illustrates the RocksDB `StorageNode`; the Postgres and
# the CogServer nodes work exactly the same way.
#
# The `FileStorageNode` implements a subset of the `StorageNode` API,
# just enough to read and write plain-ASCII (plain-UTF8) flat files
# containing s-expressions.
#
# It may be useful to review the `persist-store.scm` example before
# studying this one.
#
# -------------------------------------------------------------------
# Architectural Notes & Commentary.
#
# Some important background to be aware of.
#
# Besides the modules mentioned above, other SQL, no-SQL and graph
# databases have been tried. This has not worked out so well. It turns
# out that the cost of converting Atoms and Values to the native database
# format (serialization/deserialization) takes up far more CPU time than
# the AtomSpace does. That is, converting Atoms/Values into key-value
# pairs (for no-SQL databases) or into row/column format (for SQL) or
# even into vertices and edges (for graph databases) is a very costly
# proposition. It take a *lot* of CPU time! Worse, most databases are
# network-enabled, and thus have to create packets, send them and decode
# them at the other end, which adds even more overhead!  We found this
# out the hard way with Neo4J, which ran literally 1000x slower (that's
# right, one-thousand times slower) than what the AtomSpace can do,
# in-RAM. Performance is hard, it turns out.
#
# Another issue is that most databases provide many, many features that
# are simply not needed by the AtomSpace. For example, data analytics
# is more-or-less totally useless. You can store the AtomSpace in a DB,
# but the format defies data analytics, because the AtomSpace format
# encodes language, logic, reasoning, bio-science and other formats that
# are completely opaque to external database systems.
#
# Another issue is that many databases compete for RAM with the
# AtomSpace; since the AtomSpace is an in-RAM database itself, having
# something else competing with it for RAM is wasteful and prevents
# larger datasets from being loadable.
#
# There appear to be three issues to consider when designing apps that
# use the AtomSpace:
#
# (1) Persistence-to-disk. This is best achieved by using the RocksDB
#     module. Its really quite fast, and reasonably compact. It saves
#     the AtomSpace to a file, and you can use ordinary file-management
#     tools to copy distributed and backup RocksDB AtomSpaces. See
#     https://github.com/opencog/atomspace-rocks and the examples there.
#
# (2) Network communications. The best current system for this is the
#     CogServer-based client/server system. A single CogServer can scale
#     to approximately a dozen clients, all sharing data via the common
#     CogServer. This is a single hub-n-spoke model; by running many
#     hubs (many CogServers) one can have a crude distributed AtomSpace
#     system. Of course, two CogServers can talk to one-another in a
#     peer-to-peer fashion. See the repo at
#     https://github.com/opencog/atomspace-cog and the examples there.
#
# (3) Proxying and coordination of distributed work. Consider a
#     CogServer sitting on top of RocksDB. When a client goes to fetch
#     an Atom from the CogServer (that is, from the AtomSpace inside
#     the CogServer), it can happen that it's not there because its
#     still on disk, and hasn't been loaded yet. The ProxyNodes solve
#     this problem: they're configurable agents that can pass requests
#     on to others. They can be configured in arbitrarily complicated
#     ways, to pipe AtomSpace data around between various locations.
#
# THE END.
