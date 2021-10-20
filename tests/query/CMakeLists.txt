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
	pattern
	clearbox
	execution
	atomspace
	logger
)

# We want to run the tests in the order below;
# Each test gets progressively more complex, and exercises
# features that the later tests depend on.

ADD_CXXTEST(PatternUTest)
ADD_CXXTEST(StackUTest)
ADD_CXXTEST(BigPatternUTest)
ADD_CXXTEST(BiggerPatternUTest)
ADD_CXXTEST(LoopPatternUTest)
ADD_CXXTEST(BooleanUTest)
ADD_CXXTEST(Boolean2NotUTest)
ADD_CXXTEST(ConstantClausesUTest)
ADD_CXXTEST(PermutationsUTest)

# Unit tests for queries using VariableSet as variable declaration
ADD_CXXTEST(BindVariableSetUTest)

# These are NOT in alphabetical order; they are in order of
# simpler to more complex.  Later test cases assume features
# that are tested in earlier test cases.  DO NOT reorder this
# list unless you are sure of what you are doing.
IF (HAVE_GUILE)
	ADD_CXXTEST(PatternCrashUTest)
	ADD_CXXTEST(DisconnectedUTest)
	ADD_CXXTEST(ImplicationUTest)
	ADD_CXXTEST(QueryUTest)
	ADD_CXXTEST(ExecutionOutputUTest)
	ADD_CXXTEST(BindTVUTest)
	ADD_CXXTEST(EvalLinkDefaultTVUTest)
	ADD_CXXTEST(BuggyStackUTest)
	ADD_CXXTEST(VarTypeNotUTest)
	ADD_CXXTEST(BuggyNotUTest)
	ADD_CXXTEST(MatchLinkUTest)
	ADD_CXXTEST(UnorderedUTest)
	ADD_CXXTEST(StackMoreUTest)
	ADD_CXXTEST(CacheHitUTest)
	ADD_CXXTEST(GlobUTest)
	ADD_CXXTEST(RecognizerUTest)
	ADD_CXXTEST(ArcanaUTest)
	ADD_CXXTEST(SubstitutionUTest)
	ADD_CXXTEST(GetLinkUTest)
	ADD_CXXTEST(AnchorUTest)
	ADD_CXXTEST(NotLinkUTest)
	ADD_CXXTEST(GetStateUTest)
	ADD_CXXTEST(ClassicalBooleanUTest)
	ADD_CXXTEST(DeepTypeUTest)
	ADD_CXXTEST(TypeChoiceUTest)

	ADD_CXXTEST(GreaterThanUTest)
	ADD_CXXTEST(IsClosedUTest)
	ADD_CXXTEST(IsTrueUTest)
	ADD_CXXTEST(IsFalseUTest)
	ADD_CXXTEST(GreaterComputeUTest)
	ADD_CXXTEST(VirtualUTest)
	ADD_CXXTEST(PredicateFormulaUTest)
	ADD_CXXTEST(SequenceUTest)
	ADD_CXXTEST(EvaluationUTest)
	ADD_CXXTEST(DontExecUTest)
	ADD_CXXTEST(QuoteUTest)
	ADD_CXXTEST(UnquoteUTest)
	ADD_CXXTEST(LocalQuoteUTest)
	ADD_CXXTEST(BuggyLinkUTest)
	ADD_CXXTEST(BuggyQuoteUTest)
	ADD_CXXTEST(BuggyEqualUTest)
	ADD_CXXTEST(BuggySelfGroundUTest)
	ADD_CXXTEST(BuggyBindLinkUTest)
	ADD_CXXTEST(ChoiceLinkUTest)
	ADD_CXXTEST(DefineUTest)
	ADD_CXXTEST(FiniteStateMachineUTest)
	ADD_CXXTEST(AbsentUTest)
	ADD_CXXTEST(PresentUTest)
	ADD_CXXTEST(AlwaysUTest)
	ADD_CXXTEST(ScopeUTest)
	ADD_CXXTEST(IllPutUTest)
	ADD_CXXTEST(SudokuUTest)
	ADD_CXXTEST(EinsteinUTest)

	ADD_CXXTEST(NestedClauseUTest)

	ADD_CXXTEST(NoExceptionUTest)

	TARGET_LINK_LIBRARIES(VarTypeNotUTest
		${COGUTIL_LIBRARY}
	)

	ADD_GUILE_TEST(MeetLinkValueTest meet-link-value.scm)

ENDIF (HAVE_GUILE)

# Run only the tests in this subdirectory
ADD_CUSTOM_TARGET (qtest
	DEPENDS tests
	WORKING_DIRECTORY .
	COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
	COMMENT "Running pattern-engine tests..."
)
