INCLUDE_DIRECTORIES (
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
	logger
	${GUILE_LIBRARIES}
)

ADD_CXXTEST(BasicSCMUTest)
ADD_CXXTEST(SCMPrimitiveUTest)
ADD_CXXTEST(MultiAtomSpaceUTest)
ADD_CXXTEST(MultiThreadUTest)
ADD_CXXTEST(SCMUtilsUTest)
ADD_CXXTEST(SCMExecutionOutputUTest)

ADD_GUILE_TEST(SCMOpencogTestRunnerPass scm-opencog-test-runner-pass.scm)
ADD_GUILE_TEST(SCMOpencogTestRunnerFail scm-opencog-test-runner-fail.scm)
# The SCMOpencogTestRunnerFail fails purposely, as it is testing that the
# scheme opencog-test-runner exits properly; thus requiring the flipping
# of the test result by setting its properties.
SET_PROPERTY(TEST SCMOpencogTestRunnerFail PROPERTY WILL_FAIL TRUE)

# Configure LD_LIBRARY_PATH for loading the libraries used by the guile code,
# so as to make it possible to run tests before executing "make install".
SET_PROPERTY(TEST SCMOpencogTestRunnerPass SCMOpencogTestRunnerFail
	PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/opencog/guile")

# Guile-python bridge requires python
IF (HAVE_CYTHON)
	ADD_GUILE_TEST(SCMPython scm-python.scm)
	SET_PROPERTY(TEST SCMPython
		PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/opencog/guile:${PROJECT_BINARY_DIR}/opencog/cython")

	ADD_GUILE_TEST(SCMPythonSharedAtomspace scm-python-shared-atomspace.scm)
	SET_PROPERTY(TEST SCMPythonSharedAtomspace
		PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/opencog/guile:${PROJECT_BINARY_DIR}/opencog/atoms/execution:${PROJECT_BINARY_DIR}/opencog/cython")

	ADD_GUILE_TEST(SCMPythonArity scm-python-arity.scm)
	SET_PROPERTY(TEST SCMPythonArity
		PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/opencog/guile:${PROJECT_BINARY_DIR}/opencog/atoms/execution:${PROJECT_BINARY_DIR}/opencog/cython")

ENDIF (HAVE_CYTHON)
