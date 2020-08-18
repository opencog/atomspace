
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (parallel
	ExecuteThreadedLink.cc
	ParallelLink.cc
	ThreadJoinLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(parallel opencog_atom_types)

TARGET_LINK_LIBRARIES(parallel
	atomcore
	atombase
)

INSTALL (TARGETS parallel
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	ExecuteThreadedLink.h
	ParallelLink.h
	ThreadJoinLink.h
	DESTINATION "include/opencog/atoms/parallel"
)
