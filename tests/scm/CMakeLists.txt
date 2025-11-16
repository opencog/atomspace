INCLUDE_DIRECTORIES (
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/guile
	${PROJECT_SOURCE_DIR}/opencog/util
)

ADD_CXXTEST(BasicSCMUTest)
ADD_CXXTEST(SCMPrimitiveUTest)
ADD_CXXTEST(MultiAtomSpaceUTest)
ADD_CXXTEST(MultiThreadUTest)
ADD_CXXTEST(SCMUtilsUTest)
ADD_CXXTEST(SCMUtf8StringUTest)
ADD_CXXTEST(SCMExecutionOutputUTest)

ADD_GUILE_TEST(SCMOpencogTestRunnerPass scm-opencog-test-runner-pass.scm)
ADD_GUILE_TEST(SCMOpencogTestRunnerFail scm-opencog-test-runner-fail.scm)
# The SCMOpencogTestRunnerFail fails purposely, as it is testing that the
# scheme opencog-test-runner exits properly; thus requiring the flipping
# of the test result by setting its properties.
SET_PROPERTY(TEST SCMOpencogTestRunnerFail PROPERTY WILL_FAIL TRUE)

ADD_GUILE_TEST(CopyAtomTest copy-atom-test.scm)
ADD_GUILE_TEST(InlineValuesTest inline-values-test.scm)
ADD_GUILE_TEST(BooleanPredicateTest boolean-predicate-test.scm)

# Guile-python bridge requires python
IF (HAVE_CYTHON)
	ADD_GUILE_TEST(ScmPythonTest scm-python-test.scm)
	ADD_GUILE_TEST(ScmPythonSharedAtomspaceTest scm-python-shared-atomspace-test.scm)
	ADD_GUILE_TEST(ScmPythonArityTest scm-python-arity-test.scm)

	# Set PYTHONPATH so Python can find the opencog module in the build directory
	SET_PROPERTY(TEST ScmPythonTest
		APPEND PROPERTY ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")
	SET_PROPERTY(TEST ScmPythonSharedAtomspaceTest
		APPEND PROPERTY ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")
	SET_PROPERTY(TEST ScmPythonArityTest
		APPEND PROPERTY ENVIRONMENT "PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython")
ENDIF (HAVE_CYTHON)
