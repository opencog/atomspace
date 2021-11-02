
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (join
	JoinLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(join opencog_atom_types)

TARGET_LINK_LIBRARIES(join
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS join EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	JoinLink.h
	DESTINATION "include/opencog/atoms/join"
)
