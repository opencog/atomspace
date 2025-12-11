#
# Test applications for custom chemistry types.
#
# The chem_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# C++ test executable
ADD_EXECUTABLE(chem-types-cxx-test
   chem-types-test.cc
)

TARGET_LINK_LIBRARIES(chem-types-cxx-test
	test-chem-types
	atomspace
)

# Add C++ test
ADD_TEST(NAME ChemTypesTest
	COMMAND chem-types-cxx-test
	WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

# Add Scheme test
IF (HAVE_GUILE)
	ADD_GUILE_TEST(ChemTypesSchemeTest chem-types-test.scm)
ENDIF (HAVE_GUILE)

# Add Python test
IF (HAVE_CYTHON)
	ADD_TEST(NAME ChemTypesPythonTest
		COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/chem-types-test.py
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	)
	SET_TESTS_PROPERTIES(ChemTypesPythonTest PROPERTIES
		ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython:${CMAKE_CURRENT_BINARY_DIR}/../demo-types"
	)
ENDIF (HAVE_CYTHON)
