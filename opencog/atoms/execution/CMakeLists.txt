
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (execution
	Force.cc
	EvaluationLink.cc
	ExecutionOutputLink.cc
	Instantiator.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(execution opencog_atom_types)

TARGET_LINK_LIBRARIES(execution
	parallel
	clearbox
	atomflow
	atomcore
	atombase
)

INSTALL (TARGETS execution
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	EvaluationLink.h
	ExecutionOutputLink.h
	Force.h
	GroundedProcedureNode.h
	Instantiator.h
	DESTINATION "include/opencog/atoms/execution"
)
