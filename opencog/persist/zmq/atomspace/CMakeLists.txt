# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES (
	${CMAKE_BINARY_DIR}
)

# Generate the .pb.h and .pb.cc files dynamically.
ADD_CUSTOM_COMMAND(
	OUTPUT ZMQMessages.pb.h ZMQMessages.pb.cc
	COMMAND protoc
	ARGS --cpp_out=. -I ${CMAKE_CURRENT_SOURCE_DIR}
		${CMAKE_CURRENT_SOURCE_DIR}/ZMQMessages.proto
	DEPENDS ZMQMessages.proto
	COMMENT "Serializing with protobuf"
)

ADD_LIBRARY (zmqatoms SHARED
	ZMQMessages.pb.cc ZMQMessages.pb.h
	ProtocolBufferSerializer
	ZMQClient
	ZMQPersistSCM
)

TARGET_LINK_LIBRARIES(zmqatoms zmq)

INSTALL (TARGETS zmqatoms
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/ZMQMessages.pb.h
	ProtocolBufferSerializer.h
	ZMQClient.h
	ZMQPersistSCM.h
	DESTINATION "include/opencog/persist/zmq/atomspace"
)
