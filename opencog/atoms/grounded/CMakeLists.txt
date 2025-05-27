
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (grounded
	GroundedPredicateNode.cc
	GroundedSchemaNode.cc
	LibraryManager.cc
	LibraryRunner.cc
)

IF (HAVE_GUILE)
	TARGET_SOURCES(grounded PRIVATE DLScheme.cc)
	TARGET_SOURCES(grounded PRIVATE SCMRunner.cc)
ENDIF (HAVE_GUILE)

IF (HAVE_CYTHON)
	INCLUDE_DIRECTORIES(${Python3_INCLUDE_DIRS})
	TARGET_SOURCES(grounded PRIVATE DLPython.cc)
	TARGET_SOURCES(grounded PRIVATE PythonRunner.cc)
ENDIF (HAVE_CYTHON)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(grounded opencog_atom_types)

TARGET_LINK_LIBRARIES(grounded
	atombase
)

INSTALL (TARGETS grounded
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	GroundedPredicateNode.h
	GroundedSchemaNode.h
	LibraryManager.h
	DESTINATION "include/opencog/atoms/grounded"
)
