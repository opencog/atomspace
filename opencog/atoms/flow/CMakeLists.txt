
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomflow
	CollectionOfLink.cc
	ConcatenateLink.cc
	FilterLink.cc
	IncomingOfLink.cc
	IncrementValueLink.cc
	JsonSplitLink.cc
	KeysOfLink.cc
	LinkSignatureLink.cc
	MessagesOfLink.cc
	NumberOfLink.cc
	PromiseLink.cc
	SetValueLink.cc
	SizeOfLink.cc
	SplitLink.cc
	StreamValueOfLink.cc
	StringOfLink.cc
	TypeOfLink.cc
	ValueOfLink.cc
	ValueShimLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomflow opencog_atom_types)

TARGET_LINK_LIBRARIES(atomflow
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomflow EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	CollectionOfLink.h
	ConcatenateLink.h
	FilterLink.h
	IncomingOfLink.h
	IncrementValueLink.h
	JsonSplitLink.h
	KeysOfLink.h
	LinkSignatureLink.h
	MessagesOfLink.h
	NumberOfLink.h
	PromiseLink.h
	SetValueLink.h
	SizeOfLink.h
	SplitLink.h
	StreamValueOfLink.h
	StringOfLink.h
	TypeOfLink.h
	ValueOfLink.h
	ValueShimLink.h
	DESTINATION "include/opencog/atoms/flow"
)
