
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomflow
	FetchValueOfLink.cc
	FormulaPredicateLink.cc
	NumberOfLink.cc
	PromiseLink.cc
	SetTVLink.cc
	SetValueLink.cc
	SizeOfLink.cc
	StoreValueOfLink.cc
	StreamValueOfLink.cc
	TruthValueOfLink.cc
	TypeOfLink.cc
	ValueOfLink.cc
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
	FetchValueOfLink.h
	FormulaPredicateLink.h
	NumberOfLink.h
	PromiseLink.h
	SetTVLink.h
	SetValueLink.h
	SizeOfLink.h
	StoreValueOfLink.h
	StreamValueOfLink.h
	TruthValueOfLink.h
	TypeOfLink.h
	ValueOfLink.h
	DESTINATION "include/opencog/atoms/flow"
)
