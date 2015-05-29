
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

IF (HAVE_CYTHON)
	INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})
ENDIF (HAVE_CYTHON)

ADD_LIBRARY (execution SHARED
	EvaluationLink.cc
	ExecutionOutputLink.cc
	Instantiator.cc
	ExecSCM.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(execution opencog_atom_types)

TARGET_LINK_LIBRARIES(execution
	${Boost_SYSTEM_LIBRARY}
)

IF (HAVE_CYTHON)
	TARGET_LINK_LIBRARIES(execution PythonEval)
	TARGET_LINK_LIBRARIES(execution ${PYTHON_LIBRARIES})
ENDIF (HAVE_CYTHON)

INSTALL (TARGETS execution DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	EvaluationLink.h
	ExecutionOutputLink.h
	Instantiator.h
	DESTINATION "include/${PROJECT_NAME}/atoms/execution"
)
