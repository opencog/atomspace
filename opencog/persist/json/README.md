Atomese in JSON
---------------
Read and write UTF-8 Atomese JSON expressions. This is a collection
of utilities that take Atomese JSON expressions and convert then into
in-RAM Atoms and Values living in an AtomSpace. It is intended to make
it easy for developers to write WebApps, Javascript apps, and so on.

To allow the remote app to communicate with the AtomSpace, a small
number of special commands have been hard-coded. These commands are
just enough to interact with the AtomSpace, and nothing more.


Network API
-----------
The CogServer provides a network API to send/receive Atoms over the
internet. See https://wiki.opencog.org/w/CogServer It uses the code
here to provide a newtork interface to the JSON code here.

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
```

Now create a network connection to talk to the CogServer:
```
$ rlwrap telnet localhost 17001
help
help json
json

```
