#
# Test applications for custom chemistry types.
#
# The chem_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_CXXTEST(ChemTypesUTest)
TARGET_LINK_LIBRARIES(ChemTypesUTest
	-Wl,--no-as-needed
	test-chem-types
	-Wl,--as-needed)

# Add Scheme test
IF (HAVE_GUILE)
	ADD_GUILE_TEST(ChemTypesSchemeTest chem-types-test.scm)
ENDIF (HAVE_GUILE)

# Add Python test
IF (HAVE_CYTHON AND HAVE_PYTEST)
	ADD_TEST(ChemTypesPythonTest ${PYTEST_EXECUTABLE} -v
		${CMAKE_CURRENT_SOURCE_DIR}/chem-types-test.py)
	SET_PROPERTY(TEST ChemTypesPythonTest
		PROPERTY DEPENDS test_chempydemo)
	SET_PROPERTY(TEST ChemTypesPythonTest
		APPEND PROPERTY ENVIRONMENT
		"PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython:${CMAKE_CURRENT_BINARY_DIR}/../demo-types")
ENDIF (HAVE_CYTHON AND HAVE_PYTEST)
