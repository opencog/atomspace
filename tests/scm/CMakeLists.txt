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
	${GUILE_LIBRARIES}
)

ADD_CXXTEST(BasicSCMUTest)
ADD_CXXTEST(SCMPrimitiveUTest)
ADD_CXXTEST(MultiAtomSpaceUTest)
ADD_CXXTEST(MultiThreadUTest)
ADD_CXXTEST(SCMUtilsUTest)
ADD_CXXTEST(SCMExecutionOutputUTest)

ADD_GUILE_TEST(SCMOpenogTestRunnerPass scm-opencog-test-runner-pass.scm)
ADD_GUILE_TEST(SCMOpenogTestRunnerFail scm-opencog-test-runner-fail.scm)
# The SCMOpenogTestRunnerFail fails purposely as it is testing that the runner
# exits properly and thus flipping the result by setting the properties below.
SET_TESTS_PROPERTIES(SCMOpenogTestRunnerFail PROPERTIES WILL_FAIL TRUE)
