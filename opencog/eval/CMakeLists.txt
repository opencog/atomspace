
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY(framestack
	FrameStack.cc
)

# Avoid building this lib until AFTER the types have been built.
ADD_DEPENDENCIES(framestack opencog_atom_types)

INSTALL(TARGETS framestack
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL(FILES
	GenericEval.h
	EvaluatorPool.h
	FrameStack.h
	DESTINATION "include/opencog/eval"
)
