ENABLE_TESTING()
INCLUDE(AddCxxtest)

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
                -DPROJECT_BINARY_DIR="${CMAKE_BINARY_DIR}")

# All tests should load the atomspace scm from the build dir, unless the scm
# file is specific to the test (this variable is used by ADD_CXXTEST)
SET(GUILE_LOAD_PATH "${PROJECT_BINARY_DIR}/opencog/scm")

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# Perform tests in component-dependency order, as much as possible.
# For example, since most things depends on the atomspace,
# its is tested first.
IF (CXXTEST_FOUND)

	ADD_SUBDIRECTORY (atomspace)
	ADD_SUBDIRECTORY (atoms)

	# guile provides scheme bindings for the atomspace.
	IF (HAVE_GUILE)
		ADD_SUBDIRECTORY (scm)
	ENDIF (HAVE_GUILE)

	# Many of the query tests require a working guile, so run
	# these only after guile has been tested.
	ADD_SUBDIRECTORY (query)

	IF (HAVE_CYTHON AND HAVE_NOSETESTS)
		MESSAGE(STATUS "Found cython and nosetests, enabling python unit tests")
		ADD_SUBDIRECTORY (cython)
	ENDIF (HAVE_CYTHON AND HAVE_NOSETESTS)

	IF (HAVE_STACK)
		ADD_SUBDIRECTORY (haskell)
	ENDIF (HAVE_STACK)

ENDIF (CXXTEST_FOUND)
