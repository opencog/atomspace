
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (constrain
	ExclusiveLink.cc
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
	ExclusiveLink.h
	DESTINATION "include/opencog/atoms/constrain"
)
