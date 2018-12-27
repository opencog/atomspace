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

ADD_GUILE_TEST(SCMOpenogTestRunnerPass scm-opencog-test-runner-pass.scm)
ADD_GUILE_TEST(SCMOpenogTestRunnerFail scm-opencog-test-runner-fail.scm)
# The SCMOpenogTestRunnerFail fails purposely, as it is testing that the
# scheme opencog-test-runner exits properly; thus requiring the flipping
# of the test result by setting its properties.
SET_PROPERTY(TEST SCMOpenogTestRunnerFail PROPERTY WILL_FAIL TRUE)

# Add guile code to the load path to make it possible run tests before
# executing "make install"
SET_PROPERTY(TEST SCMOpenogTestRunnerPass SCMOpenogTestRunnerFail
	PROPERTY ENVIRONMENT "GUILE_LOAD_PATH=${PROJECT_SOURCE_DIR}/opencog/scm")

# And some more tests
IF (HAVE_CYTHON)
	ADD_GUILE_TEST(SCMOpenogPython scm-opencog-python.scm)
	SET_PROPERTY(TEST SCMOpenogPython
		PROPERTY ENVIRONMENT "GUILE_LOAD_PATH=${PROJECT_SOURCE_DIR}/opencog/scm")
	SET_PROPERTY(TEST SCMOpenogPython
		PROPERTY ENVIRONMENT "LTDL_LIBRARY_PATH=${PROJECT_BINARY_DIR}/opencog/cython:${PROJECT_BINARY_DIR}/opencog/guile")
ENDIF (HAVE_CYTHON)
