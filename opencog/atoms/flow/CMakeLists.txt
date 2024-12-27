
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomflow
	CollectionOfLink.cc
	ConcatenateLink.cc
	FilterLink.cc
	FormulaPredicateLink.cc
	IncomingOfLink.cc
	LinkSignatureLink.cc
	NumberOfLink.cc
	PromiseLink.cc
	SetTVLink.cc
	SetValueLink.cc
	SizeOfLink.cc
	StreamValueOfLink.cc
	StringOfLink.cc
	TruthValueOfLink.cc
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
	FormulaPredicateLink.h
	IncomingOfLink.h
	LinkSignatureLink.h
	NumberOfLink.h
	PromiseLink.h
	SetTVLink.h
	SetValueLink.h
	SizeOfLink.h
	StreamValueOfLink.h
	StringOfLink.h
	TruthValueOfLink.h
	TypeOfLink.h
	ValueOfLink.h
	ValueShimLink.h
	DESTINATION "include/opencog/atoms/flow"
)
