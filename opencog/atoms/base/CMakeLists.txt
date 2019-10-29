
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atombase
	Atom.cc
	ClassServer.cc
	Handle.cc
	Link.cc
	Node.cc
	Valuation.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atombase opencog_atom_types value)

TARGET_LINK_LIBRARIES(atombase
	atom_types
	value
	truthvalue
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atombase EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Atom.h
	ClassServer.h
	Handle.h
	Link.h
	Node.h
	Valuation.h
	DESTINATION "include/opencog/atoms/base"
)
