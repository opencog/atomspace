
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomflow
	AtomSpaceOfLink.cc
	CollectionOfLink.cc
	ConcatenateLink.cc
	DrainLink.cc
	FilterLink.cc
	GlobMatch.cc
	IncomingOfLink.cc
	IncrementValueLink.cc
	JsonSplitLink.cc
	KeysOfLink.cc
	LinkSignatureLink.cc
	MessagesOfLink.cc
	SetValueLink.cc
	SizeOfLink.cc
	SplitLink.cc
	StreamValueOfLink.cc
	TypeOfLink.cc
	ValueOfLink.cc
	ValueShimLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomflow opencog_atom_types)

TARGET_LINK_LIBRARIES(atomflow
	signature
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomflow EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AtomSpaceOfLink.h
	CollectionOfLink.h
	ConcatenateLink.h
	DrainLink.h
	FilterLink.h
	GlobMatch.h
	IncomingOfLink.h
	IncrementValueLink.h
	JsonSplitLink.h
	KeysOfLink.h
	LinkSignatureLink.h
	MessagesOfLink.h
	SetValueLink.h
	SizeOfLink.h
	SplitLink.h
	StreamValueOfLink.h
	TypeOfLink.h
	ValueOfLink.h
	ValueShimLink.h
	DESTINATION "include/opencog/atoms/flow"
)
