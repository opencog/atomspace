
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

IF (HAVE_CYTHON)
	INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})
ENDIF (HAVE_CYTHON)

ADD_LIBRARY (grounded
	DLScheme.cc
	GroundedPredicateNode.cc
	GroundedSchemaNode.cc
	LibraryManager.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(grounded opencog_atom_types)

TARGET_LINK_LIBRARIES(grounded
	atombase
)

IF (HAVE_CYTHON)
	TARGET_LINK_LIBRARIES(grounded PythonEval)
	TARGET_LINK_LIBRARIES(grounded ${PYTHON_LIBRARIES})
ENDIF (HAVE_CYTHON)

INSTALL (TARGETS grounded
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	GroundedPredicateNode.h
	GroundedSchemaNode.h
	LibraryManager.h
	DESTINATION "include/opencog/atoms/grounded"
)