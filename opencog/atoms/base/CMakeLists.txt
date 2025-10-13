
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atombase
	Atom.cc
	ClassServer.cc
	Handle.cc
	Link.cc
	Node.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atombase opencog_atom_types value)

TARGET_LINK_LIBRARIES(atombase
	atom_types
	value
	${COGUTIL_LIBRARY}
)

# This sets the compiler flags for the users of the AtomSpace.
# Without these, when they include Atom.h, it will be configured
# all wrong and the resulting code will core dump.
MESSAGE(STATUS "Compile time defs are: ${COMPILE_TIME_DEFS}")
SET_TARGET_PROPERTIES(atombase PROPERTIES
	INTERFACE_COMPILE_DEFINITIONS "${COMPILE_TIME_DEFS}")

INSTALL (TARGETS atombase EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Atom.h
	ClassServer.h
	Handle.h
	Link.h
	Node.h
	DESTINATION "include/opencog/atoms/base"
)
