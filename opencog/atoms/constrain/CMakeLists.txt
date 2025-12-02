
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (constrain
	AbsentLink.cc
	AlphaEqualLink.cc
	DeleteLink.cc
	EqualLink.cc
	ExclusiveLink.cc
	IdenticalLink.cc
	PresentLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(constrain opencog_atom_types)

TARGET_LINK_LIBRARIES(constrain
	execution
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS constrain EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AbsentLink.h
	AlphaEqualLink.h
	DeleteLink.h
	EqualLink.h
	ExclusiveLink.h
	IdenticalLink.h
	PresentLink.h
	DESTINATION "include/opencog/atoms/constrain"
)
