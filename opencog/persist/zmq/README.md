# persist/zmq

## Dependencies

libzmq 3.x

Ubuntu: `sudo apt-get install protobuf-compiler libzmq3-dev`

## protobuf

    protoc --proto_path=opencog/persist/zmq/atomspace --cpp_out=opencog/persist/zmq/atomspace opencog/persist/zmq/atomspace/ZMQMessages.proto

TODO: proto should be part of the build process so that the `.pb.h` and `.pb.cc`
files are updated dynamically, rather than being included in the repository.

## atomspace_needfixing

Parts of `persist/zmq` which still need fixing.
After these are fixed please move to working directory `atomspace`.
