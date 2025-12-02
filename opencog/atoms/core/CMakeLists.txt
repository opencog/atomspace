
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomcore
	Checkers.cc
	CondLink.cc
	FunctionLink.cc
	NumberNode.cc
	RandomChoice.cc
	TimeLink.cc
	UnorderedLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomcore opencog_atom_types)

TARGET_LINK_LIBRARIES(atomcore
	free
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomcore EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	CondLink.h
	FunctionLink.h
	NumberNode.h
	RandomChoice.h
	TimeLink.h
	UnorderedLink.h
	DESTINATION "include/opencog/atoms/core"
)
