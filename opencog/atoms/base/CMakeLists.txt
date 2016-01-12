
OPENCOG_ADD_ATOM_TYPES(atom_types.script 
  atom_types.h
  atom_types.definitions
  atom_types.inheritance
  core_types.scm
  core_types.pyx)
ADD_CUSTOM_TARGET(opencog_atom_types DEPENDS atom_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atombase SHARED
	atom_types.h
	Atom.cc
	ClassServer.cc
	Handle.cc
	Link.cc
	Node.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atombase opencog_atom_types)

TARGET_LINK_LIBRARIES(atombase
	truthvalue  
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atombase
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Atom.h
	${CMAKE_CURRENT_BINARY_DIR}/atom_types.h
	atom_types.cc
	ClassServer.h
	Handle.h
	Link.h
	Node.h
	types.h
	DESTINATION "include/opencog/atoms/base"
)

# Install the auto-gened atom types as well
INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/core_types.scm
   DESTINATION "${DATADIR}/scm"
)
