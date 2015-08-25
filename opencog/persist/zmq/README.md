# persist/zmq

## Dependencies

libzmq 3.x

Ubuntu: `sudo apt-get install protobuf-compiler libzmq3-dev`

TODO: use libzmqpp-dev or better yet, [azmq (Boost-friendly)](https://github.com/zeromq/azmq) for ZeroMQ 4.0+.

## protobuf

    protoc --proto_path=opencog/persist/zmq/atomspace --cpp_out=opencog/persist/zmq/atomspace opencog/persist/zmq/atomspace/ZMQMessages.proto

TODO: proto should be part of the build process so that the `.pb.h` and `.pb.cc`
files are updated dynamically, rather than being included in the repository.


## Neo4j

Neo4j Backing Store server can be built in https://github.com/ceefour/opencog-neo4j
(TODO: migrate this repository into opencog organization when more mature).

AtomSpace ZMQBackingStore and ZMQPersistSCM will connect to Neo4j Backing Store
using ZeroMQ and AtomSpace's Protobuf as messaging/persistence protocol.


## SCM

ZMQPersistSCM can be used to manage the ZMQBackingStore connection inside cogserver
guile shell:

* `zmq-open`
* `zmq-close`
* `zmq-load`
* `zmq-store`


## atomspace_needfixing

Parts of `persist/zmq` which still need fixing.
After these are fixed please move to working directory `atomspace`.
