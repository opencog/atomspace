
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (container
	JoinLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(container opencog_atom_types)

TARGET_LINK_LIBRARIES(container
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS container EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	JoinLink.h
	DESTINATION "include/opencog/atoms/container"
)
