INCLUDE_DIRECTORIES(
	${PYTHON_INCLUDE_PATH}
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/guile
	${PROJECT_SOURCE_DIR}/opencog/util
)

LINK_DIRECTORIES(
	${PROJECT_BINARY_DIR}/opencog/atomspace
	${PROJECT_BINARY_DIR}/opencog/guile
	${PROJECT_BINARY_DIR}/opencog/util
)

LINK_LIBRARIES(
	atomspace
	smob
	clearbox
	execution
	${GUILE_LIBRARIES}
)

# The PythonEvalUTest tests PythonEval independent of the CogServer. It depends
# on the server being around for the Scheme code but it does not message
# through a "py-eval" server request.
#
ADD_CXXTEST(PythonEvalUTest)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/cython/pythoneval.conf
	${PROJECT_BINARY_DIR}/tests/cython/pythoneval.conf)

TARGET_LINK_LIBRARIES(PythonEvalUTest
	atomspace
	clearbox
	smob
	PythonEval
	atomspace_cython
	${COGUTIL_LIBRARY}
)
SET_TESTS_PROPERTIES(PythonEvalUTest
	PROPERTIES
	ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
	ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")


# The PythonUtilitiesUTest tests C++ utility functions used from Cython bindings
# outside of those bindings so failures are easier to debug.
#
ADD_CXXTEST(PythonUtilitiesUTest)

TARGET_LINK_LIBRARIES(PythonUtilitiesUTest
	smob
	PythonEval
	utilities_cython
	atomspace_cython
	atomspace
	${COGUTIL_LIBRARY}
)
SET_TESTS_PROPERTIES(PythonUtilitiesUTest
	PROPERTIES
	ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
	ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")


IF (HAVE_NOSETESTS)

	# Basic test, just does the cython wrapper for opencog/util
	ADD_TEST(Cython ${NOSETESTS_EXECUTABLE} -vs
		${CMAKE_SOURCE_DIR}/tests/cython/)
	SET_TESTS_PROPERTIES(Cython
		PROPERTIES
		ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
		ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")

	IF (HAVE_ATOMSPACE)
		ADD_TEST(CythonAtomSpace ${NOSETESTS_EXECUTABLE} -vs
			${CMAKE_SOURCE_DIR}/tests/cython/atomspace/)
		SET_TESTS_PROPERTIES(CythonAtomSpace
			PROPERTIES
			ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
			ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")

		ADD_TEST(CythonUtilities ${NOSETESTS_EXECUTABLE} -vs
			${CMAKE_SOURCE_DIR}/tests/cython/utilities/)
		SET_TESTS_PROPERTIES(CythonUtilities
			PROPERTIES
			ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
			ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")

		ADD_TEST(CythonBindlink ${NOSETESTS_EXECUTABLE} -vs
			${CMAKE_SOURCE_DIR}/tests/cython/bindlink/)
		SET_TESTS_PROPERTIES(CythonBindlink
			PROPERTIES
			ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
			ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython:${PROJECT_SOURCE_DIR}/opencog/python")
	ENDIF (HAVE_ATOMSPACE)

	IF (HAVE_ATOMSPACE AND HAVE_GUILE)
		ADD_TEST(CythonGuile ${NOSETESTS_EXECUTABLE} -vs
			${CMAKE_SOURCE_DIR}/tests/cython/guile/)
		SET_TESTS_PROPERTIES(CythonGuile
			PROPERTIES
			ENVIRONMENT "PYTHONDONTWRITEBYTECODE=dont-do-it"
			ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")
	ENDIF (HAVE_ATOMSPACE AND HAVE_GUILE)

ENDIF (HAVE_NOSETESTS)
