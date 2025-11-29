
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (scope
	LambdaLink.cc
	PutLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(scope opencog_atom_types)

TARGET_LINK_LIBRARIES(scope
	atomcore
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS scope EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	LambdaLink.h
	PutLink.h
	DESTINATION "include/opencog/atoms/scope"
)
