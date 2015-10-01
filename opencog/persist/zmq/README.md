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

## cogserver

Ensure in `lib/opencog.conf` you have this line:

	MODULES               = opencog/server/libbuiltinreqs.so,
	                        opencog/modules/libPersistModule.so,
	                        opencog/modules/libPersistZmqModule.so,
	                        ...

Make sure `listmodules` command shows:

	Filename: libPersistZmqModule.so, ID: opencog::PersistZmqModule

and `help` command shows:

	zmq-close:          Close the ZeroMQ persistence
	zmq-load:           Load contents of ZeroMQ persistence
	zmq-open:           Open connection to ZeroMQ persistence
	zmq-store:          Save the atomtable on the ZeroMQ persistence

If any problem, check `/tmp/cogserver.log` (this path is configured in `lib/opencog.conf`)

## Testing

1. In `atomspace_build`, do `make -j4` and `sudo make install`
2. In `opencog_build`, do `make -j4`
3. In `opencog_build`, run: `opencog/server/cogserver`
4. Telnet to localhost port 17001: `telnet localhost 17001`
5. `zmq-open tcp://127.0.0.1:5555`
5. Go into Scheme shell: `scm`
6. Create a node: `(ConceptNode "human")`

## TODO/Problems

When store():

	[2015-08-27 07:00:31:533] [ERROR] Caught signal 11 (Segmentation fault) on thread 140021036828736
	        Stack Trace:
	        2: Logger.h:175   opencog::Logger::Base::~Base()
	        3: CogServerMain.cc:79  _Z7sighandi()
	        4: ??:0 killpg()
	        5: ZMQMessages.pb.cc:2682         ZMQAtomMessage::IsInitialized() const
	        6: ZMQMessages.pb.cc:3736         ZMQRequestMessage::IsInitialized() const
	        7: ??:0   google::protobuf::MessageLite::AppendToString(std::string*) const
	        8: ??:0   google::protobuf::MessageLite::SerializeAsString() const
	        9: ZMQClient.cc:70        opencog::ZMQClient::sendMessage(ZMQRequestMessage&, ZMQReplyMessage&)
	        10: ZMQClient.cc:104      opencog::ZMQClient::storeAtom(std::shared_ptr<opencog::Atom> const&, bool)
	        11: ZMQClient.cc:128      opencog::ZMQClient::storeSingleAtom(std::shared_ptr<opencog::Atom>)
	        12: ZMQClient.cc:132      opencog::ZMQClient::store_cb(std::shared_ptr<opencog::Atom>)
	        13: ZMQClient.cc:144    operator()()
	        14: AtomTable.h:230     operator()()
	        15: stl_algo.h:4417     for_each<opencog::TypeIndex::iterator, opencog::AtomTable::foreachHandleByType(Function, opencog::Type, bool, bool) const [with Function = opencog::ZMQClient::store(const opencog::AtomTable&)::__lambda1; opencog::Type = short unsigned int]::__lambda0>()
	        16: AtomTable.h:231     foreachHandleByType<opencog::ZMQClient::store(const opencog::AtomTable&)::__lambda1>()
	        17: ZMQClient.cc:145      opencog::ZMQClient::store(opencog::AtomTable const&)
	        18: ZMQPersistSCM.cc:221          opencog::ZMQPersistSCM::do_store()
	        19: PersistZmqModule.cc:131       opencog::PersistZmqModule::do_store(opencog::Request*, std::list<std::string, std::allocator<std::string> >)
	        20: PersistZmqModule.h:71         opencog::PersistZmqModule::do_storeRequest::execute()
	        21: CogServer.cc:297      opencog::CogServer::processRequests()
	        22: CogServer.cc:237      opencog::CogServer::runLoopStep()
	        23: CogServer.cc:200      opencog::CogServer::serverLoop()
	        24: CogServerMain.cc:211        main()
	        25: ??:0        __libc_start_main()
	        26: ??:0        _start()
