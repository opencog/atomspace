# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES (
	${CMAKE_BINARY_DIR}
)

# Proto should be part of the build process so that the .pb.h and .pb.cc
# files are updated dynamically, rather than being included in the
# repository.
#
ADD_LIBRARY (zmqatoms SHARED
	ZMQMessages.pb
	ProtocolBufferSerializer
	ZMQClient
	ZMQPersistSCM
)

TARGET_LINK_LIBRARIES(zmqatoms zmq)

INSTALL (TARGETS zmqatoms
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ZMQMessages.pb.h
	ProtocolBufferSerializer.h
	ZMQClient.h
	ZMQPersistSCM.h
	DESTINATION "include/opencog/persist/zmq/atomspace"
)
