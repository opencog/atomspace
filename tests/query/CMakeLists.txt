INCLUDE_DIRECTORIES (
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/query
	${PROJECT_SOURCE_DIR}/opencog/util
)

LINK_DIRECTORIES (
	${PROJECT_BINARY_DIR}/opencog/atomspace
	${PROJECT_BINARY_DIR}/opencog/query
	${PROJECT_BINARY_DIR}/opencog/util
)

LINK_LIBRARIES (
	query
	atomspace
	clearbox
	execution
)

#start gearman-job-server

# We want to run the tests in the order below;
# Each test gets progressively more complex, and exercises
# features that the later tests depend on.

ADD_CXXTEST(PatternUTest)
ADD_CXXTEST(PatternCrashUTest)
ADD_CXXTEST(StackUTest)
ADD_CXXTEST(BigPatternUTest)
ADD_CXXTEST(BiggerPatternUTest)
ADD_CXXTEST(LoopPatternUTest)
ADD_CXXTEST(BooleanUTest)
ADD_CXXTEST(Boolean2NotUTest)
ADD_CXXTEST(FuzzyPatternUTest)


# Its a *lot* easier to write scheme, than to write C++ code!
# These are not in alphabetical order; they are in order of
# simpler to more complex.
IF (HAVE_GUILE)
	IF (HAVE_GEARMAN)
		ADD_CXXTEST(DistScmUTest)
	ENDIF (HAVE_GEARMAN)
	ADD_CXXTEST(DisconnectedUTest)
	ADD_CXXTEST(ImplicationUTest)
	ADD_CXXTEST(ExecutionOutputUTest)
	ADD_CXXTEST(BuggyStackUTest)
	ADD_CXXTEST(VarTypeNotUTest)
	ADD_CXXTEST(BuggyNotUTest)
	ADD_CXXTEST(MatchLinkUTest)
	ADD_CXXTEST(UnorderedUTest)
	ADD_CXXTEST(StackMoreUTest)
	ADD_CXXTEST(ArcanaUTest)
	ADD_CXXTEST(SubstitutionUTest)
	ADD_CXXTEST(GetLinkUTest)

	ADD_CXXTEST(GreaterThanUTest)
	ADD_CXXTEST(GreaterComputeUTest)
	ADD_CXXTEST(SequenceUTest)
	ADD_CXXTEST(EvaluationUTest)
	ADD_CXXTEST(QuoteUTest)
	ADD_CXXTEST(BuggyLinkUTest)
	ADD_CXXTEST(BuggyQuoteUTest)
	ADD_CXXTEST(BuggyEqualUTest)
	ADD_CXXTEST(BuggySelfGroundUTest)
	ADD_CXXTEST(ChoiceLinkUTest)
	ADD_CXXTEST(DefineUTest)
	ADD_CXXTEST(FiniteStateMachineUTest)
	ADD_CXXTEST(AbsentUTest)
	ADD_CXXTEST(SingleUTest)
	ADD_CXXTEST(AttentionalFocusCBUTest)
	ADD_CXXTEST(SudokuUTest)
	ADD_CXXTEST(EinsteinUTest)

	TARGET_LINK_LIBRARIES(VarTypeNotUTest
		${COGUTIL_LIBRARY}
	)
ENDIF (HAVE_GUILE)

# Run only the tests in this subdirectory
ADD_CUSTOM_TARGET (qtest
	DEPENDS tests
	WORKING_DIRECTORY .
	COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
	COMMENT "Running pattern-matcher tests..."
)

# Copy files to the build directory.  We do this so that the
# scheme files can be found no matter where the source directory
# might be.
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/buggy-crime.scm
    ${PROJECT_BINARY_DIR}/tests/query/buggy-crime.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/buggy-link.scm
    ${PROJECT_BINARY_DIR}/tests/query/buggy-link.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/buggy-stack.scm
    ${PROJECT_BINARY_DIR}/tests/query/buggy-stack.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/buggy-selfgnd.scm
    ${PROJECT_BINARY_DIR}/tests/query/buggy-selfgnd.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-disconnected.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-disconnected.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-embed.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-embed.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-embed-disco.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-embed-disco.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-link.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-link.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-nest.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-nest.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-top-nest.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-top-nest.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-typed.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-typed.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/choice-unary.scm
    ${PROJECT_BINARY_DIR}/tests/query/choice-unary.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/define.scm
    ${PROJECT_BINARY_DIR}/tests/query/define.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/define-schema.scm
    ${PROJECT_BINARY_DIR}/tests/query/define-schema.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/disco-vars.scm
    ${PROJECT_BINARY_DIR}/tests/query/disco-vars.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/evaluation.scm
    ${PROJECT_BINARY_DIR}/tests/query/evaluation.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/finite-state-machine.scm
    ${PROJECT_BINARY_DIR}/tests/query/finite-state-machine.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/get-link.scm
    ${PROJECT_BINARY_DIR}/tests/query/get-link.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/get-link-eval.scm
    ${PROJECT_BINARY_DIR}/tests/query/get-link-eval.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/greater-compute.scm
    ${PROJECT_BINARY_DIR}/tests/query/greater-compute.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/greater_than.scm
    ${PROJECT_BINARY_DIR}/tests/query/greater_than.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/match-link.scm
    ${PROJECT_BINARY_DIR}/tests/query/match-link.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/sequence.scm
    ${PROJECT_BINARY_DIR}/tests/query/sequence.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/seq-presence.scm
    ${PROJECT_BINARY_DIR}/tests/query/seq-presence.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/single.scm
    ${PROJECT_BINARY_DIR}/tests/query/single.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/stackmore-o-o.scm
    ${PROJECT_BINARY_DIR}/tests/query/stackmore-o-o.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/stackmore-o-u.scm
    ${PROJECT_BINARY_DIR}/tests/query/stackmore-o-u.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/stackmore-u-o.scm
    ${PROJECT_BINARY_DIR}/tests/query/stackmore-u-o.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/stackmore-u-u.scm
    ${PROJECT_BINARY_DIR}/tests/query/stackmore-u-u.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/substitution.scm
    ${PROJECT_BINARY_DIR}/tests/query/substitution.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/test_types.scm
    ${PROJECT_BINARY_DIR}/tests/query/test_types.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/unordered.scm
    ${PROJECT_BINARY_DIR}/tests/query/unordered.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/unordered-more.scm
    ${PROJECT_BINARY_DIR}/tests/query/unordered-more.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/unordered-exhaust.scm
    ${PROJECT_BINARY_DIR}/tests/query/unordered-exhaust.scm)
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/query/var-type-not.scm
    ${PROJECT_BINARY_DIR}/tests/query/var-type-not.scm)
