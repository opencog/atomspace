Atomese in JSON
===============
Read and write UTF-8 Atomese JSON expressions. This is a collection
of utilities that take Atomese JSON expressions and convert then into
in-RAM Atoms and Values living in an AtomSpace. It is intended to make
it easy for developers to write WebApps, Javascript apps, and so on.

To allow the remote app to communicate with the AtomSpace, a small
number of special commands have been hard-coded. These commands are
just enough to interact with the AtomSpace, and nothing more.

Status
------
**Version 0.9.0.** There is just enough here to be usable for basic things.
Several convenience calls are missing, as well as the ability to run
arbitrary queries. Adding support for this is **really easy**, and just
slightly tedious.  The hard part is writing the docs!
Patches are solicited and will be accepted.


Network API
-----------
The CogServer provides a network API to send/receive Atoms over the
internet. See https://wiki.opencog.org/w/CogServer It uses the code
here to provide a network interface to the JSON code here.

Examples
--------
First, create an AtomSpace, put some atoms into it, and start the
CogServer. This is as usual, starting at the `bash` prompt: 
```
$ guile
(use-modules (opencog) (opencog cogserver))
(start-cogserver)
(List (Concept "foo") (Concept "bar"))
(cog-set-value! (ConceptNode "foo") (Predicate "key") (FloatValue 1 2 3))
(cog-set-tv! (ConceptNode "foo") (SimpleTruthValue 0.3 0.8))
(cog-set-tv! (ConceptNode "bar") (stv 0.4 0.5))
```

Now create a network connection to talk to the CogServer, and send it
an assortment of "javascript" commands. The replies will be in JSON.
```
$ rlwrap telnet localhost 17001
help
help json
json
AtomSpace.getAtoms("Node")  // Get all Nodes, including ConceptNodes
AtomSpace.getAtoms("Node", false) // Get only Nodes
AtomSpace.getAtoms("Atom", true)  // Get all Atoms
AtomSpace.haveNode("Concept", "foo")    // Yes, that exists
AtomSpace.haveNode("Concept", "foobooboo")  // Does not exist
AtomSpace.haveLink("List", [{"type": "Concept", "name": "foo"}]) // Nope.
AtomSpace.haveLink("List", [{"type": "Concept", "name": "foo"}, {"type": "Concept", "name": "bar"}]) // Yes.
AtomSpace.haveAtom({"type": "List", "outgoing":[{"type": "Concept", "name": "foo"}, {"type": "Concept", "name": "bar"}]}) // Yes
AtomSpace.haveAtom({"type": "List", "outgoing":[{"type": "Concept", "name": "ding"}, {"type": "Concept", "name": "dong"}]}) // No
AtomSpace.haveAtom({"type": "Concept", "name": "foo"}) // Yes
AtomSpace.getIncoming({"type": "Concept", "name": "foo"}) // Incoming set
AtomSpace.getIncoming({"type": "Concept", "name": "foo"}, "List") // OK
AtomSpace.getIncoming({"type": "Concept", "name": "foo"}, "EvaluationLink") // Empty
AtomSpace.getIncoming({"type": "Concept", "name": "bbb"}) // Empty list
AtomSpace.getValues({ "type": "Concept", "name": "foo"})  // All values
AtomSpace.getValues({ "type": "Concept", "name": "bar"})  // All values
```

### Example output:
```
json> AtomSpace.getIncoming({"type": "Concept", "name": "foo"})

[{
  "type": "ListLink",
  "outgoing": [
    {
      "type": "ConceptNode",
      "name": "foo"
    },
    {
      "type": "ConceptNode",
      "name": "bar"
    }]}]
```

### Other commands
These include:
* Create an Atom. Returns `true` if successful, else `false`.
```
AtomSpace.makeAtom({"type": "Concept", "name": "foo"})
```

* Get the TruthValue on Atom. Returns the value.
```
AtomSpace.getTV({"type": "Concept", "name": "foo"})
```
* Set the TruthValue on Atom. Returns true if successful, else false.
```
AtomSpace.setTV({ "type": "ConceptNode", "name": "foo", "value": { "type": "SimpleTruthValue", "value": [0.3, 0.4] } } )
```

* Set arbitrary Value on Atom. Returns true if successful, else false.
```
AtomSpace.setValue({ "type": "ConceptNode", "name": "foo", "key": { "type": "PredicateNode", "name": "keewee" }, "value": { "type": "FloatValue", "value": [1, 2, 3] } } )

AtomSpace.setValue({ "type": "ConceptNode", "name": "foo", "key": { "type": "PredicateNode", "name": "strkey" }, "value": { "type": "StringValue", "value": ["a", "b", "c, \"d\", e"] } } )

AtomSpace.setValue({ "type": "ConceptNode", "name": "foo", "key": { "type":
"PredicateNode", "name": "linky" }, "value": { "type": "LinkValue", "value": [
{ "type": "FloatValue", "value": [4, 5, 6] },
{ "type": "StringValue", "value": ["g", "h", "i, \"j\", k"] },
{ "type": "CountTruthValue", "value": [7, 8, 9] } } } )
```

* Get base and derived types.
```
AtomSpace.getSubTypes("Link")
AtomSpace.getSuperTypes("ListLink")
```

* Execute an executable Atom
```
AtomSpace.execute({ "type": "PlusLink",
    "outgoing":
        [{ "type": "NumberNode", "name": "2" },
         { "type": "NumberNode", "name": "2" }] })
```


### Unimplemented Commands
* Get Incoming set by Type -- this save a lot of I/O for many cases.
* Set multiple values at once -- this would be a nice-to-have utility.
* Extract atoms
* Wrapper for cog-evaluate!

### General Access
To make the AtomSpace generically usable, even more is needed.
Frequently-used functions include the following:

* StorageNode API -- for generic dataset access.
* Run arbitrary scheme, python

Should the above be added to the JSON API? Probably not. This starts to
make the JSON API to look more and more like a JavaScript API. The
correct answer would be to bite the bullet, and create an actual
JavaScript API, and also a cogserver plugin that provides network access
to it.

General Limitations
-------------------
At this time, the JSON parsing is simplistic, and possibly buggy. The
following limitations apply:
* There must not be any newlines in the data sent to the server;
  commands must be on one line.
* The order of the tags must be as documented above. So, for example,
  to specity an Atom, the Atom type must come first.


JavaScript API
--------------
Version 0.0.0 of a JavaScript API for this thing can be found at
https://github.com/opencog/atomspace-js
