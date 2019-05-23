
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

IF (HAVE_CYTHON)
	INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})
ENDIF (HAVE_CYTHON)

ADD_LIBRARY (execution
	DLScheme.cc
	Force.cc
	EvaluationLink.cc
	ExecutionOutputLink.cc
	Instantiator.cc
	MapLink.cc
	LibraryManager.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(execution opencog_atom_types)

TARGET_LINK_LIBRARIES(execution
	atombase
	atomcore
	clearbox
)

IF (HAVE_CYTHON)
	TARGET_LINK_LIBRARIES(execution PythonEval)
	TARGET_LINK_LIBRARIES(execution ${PYTHON_LIBRARIES})
ENDIF (HAVE_CYTHON)

INSTALL (TARGETS execution
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Force.h
	EvaluationLink.h
	ExecutionOutputLink.h
	Instantiator.h
	DESTINATION "include/opencog/atoms/execution"
)

IF (HAVE_GUILE)
	ADD_LIBRARY (exec ExecSCM.cc)
	TARGET_LINK_LIBRARIES(exec execution smob)
	INSTALL (TARGETS exec
		EXPORT AtomSpaceTargets
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
ENDIF (HAVE_GUILE)

# This does not belong, but cannot be fixed until the Python bindings
# are fixed, first.
INSTALL (FILES
	MapLink.h
	DESTINATION "include/opencog/atoms/core"
)
