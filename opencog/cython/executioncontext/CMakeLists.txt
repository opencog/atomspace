
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (executioncontext
	Context.cc
)

# Avoid building this lib until AFTER the types have been built.
ADD_DEPENDENCIES(executioncontext opencog_atom_types)

INSTALL (TARGETS executioncontext
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Context.h
	DESTINATION "include/opencog/executioncontext"
)
