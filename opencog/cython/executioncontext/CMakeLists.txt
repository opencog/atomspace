
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})


ADD_LIBRARY (executioncontext
	Context.cc
)


TARGET_LINK_LIBRARIES(executioncontext
)


INSTALL (TARGETS executioncontext
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Context.h
	DESTINATION "include/opencog/executioncontext"
)
