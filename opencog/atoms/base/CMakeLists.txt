
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
ADD_DEPENDENCIES(atombase opencog_atom_types atomproto)

TARGET_LINK_LIBRARIES(atombase
	atomproto
	truthvalue
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atombase
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
