ENABLE_TESTING()
INCLUDE(AddCxxtest)

IF (WIN32)
	ADD_DEFINITIONS(-D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES
	                -D_CRT_SECURE_NO_WARNINGS)
ENDIF (WIN32)

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR=\\"${CMAKE_SOURCE_DIR}\\"
                -DPROJECT_BINARY_DIR=\\"${CMAKE_BINARY_DIR}\\")

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# Perform tests in component-dependency order, as much as possible.
# For example, since most things depends on the atomspace,
# its is tested first.
IF (CXXTEST_FOUND)

	ADD_SUBDIRECTORY (truthvalue)
	ADD_SUBDIRECTORY (atomspace)
	ADD_SUBDIRECTORY (atoms)
	ADD_SUBDIRECTORY (atomutils)
	ADD_SUBDIRECTORY (unify)
	ADD_SUBDIRECTORY (atomspaceutils)
	ADD_SUBDIRECTORY (attentionbank)

	# Persistence is for saving/restoring atomspace to disk.
	ADD_SUBDIRECTORY (persist)

	# guile provides scheme bindings for the atomspace.
	ADD_SUBDIRECTORY (scm)
	ADD_SUBDIRECTORY (matrix)

	# Many of the query tests require a working guile, so run
	# these only after guile has been tested.
	ADD_SUBDIRECTORY (query)

	# Reasoning depends on query, above.
	ADD_SUBDIRECTORY (rule-engine)

	IF (HAVE_CYTHON AND PYTHONINTERP_FOUND)
		ADD_SUBDIRECTORY (cython)
	ENDIF (HAVE_CYTHON AND PYTHONINTERP_FOUND)

    IF (HAVE_STACK)
         ADD_SUBDIRECTORY (haskell)
    ENDIF (HAVE_STACK)

ENDIF (CXXTEST_FOUND)
