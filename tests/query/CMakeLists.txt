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
ADD_CXXTEST(PermutationsUTest)

IF (HAVE_GUILE)
	LINK_LIBRARIES(smob)
	ADD_CXXTEST(ConstantClausesUTest)
ENDIF (HAVE_GUILE)

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
	ADD_CXXTEST(SparseUTest)
	ADD_CXXTEST(StackMoreUTest)
	ADD_CXXTEST(CacheHitUTest)
	ADD_CXXTEST(GlobUTest)
	ADD_CXXTEST(RecognizerUTest)
	ADD_CXXTEST(ArcanaUTest)
	ADD_CXXTEST(SubstitutionUTest)
	ADD_CXXTEST(GetLinkUTest)
	ADD_CXXTEST(NotLinkUTest)
	ADD_CXXTEST(GetStateUTest)
	ADD_CXXTEST(ClassicalBooleanUTest)
	ADD_CXXTEST(DeepTypeUTest)
ENDIF (HAVE_GUILE)

ADD_CXXTEST(TypeChoiceUTest)

IF (HAVE_GUILE)
	ADD_CXXTEST(GreaterThanUTest)
	ADD_CXXTEST(IsClosedUTest)
	ADD_CXXTEST(IsTrueUTest)
	ADD_CXXTEST(IsFalseUTest)
	ADD_CXXTEST(GreaterComputeUTest)
	ADD_CXXTEST(VirtualUTest)
	ADD_CXXTEST(FormulaPredicateUTest)
	ADD_CXXTEST(SequenceUTest)
	ADD_CXXTEST(EvaluationUTest)
	ADD_CXXTEST(DontExecUTest)
	ADD_CXXTEST(QuoteUTest)
ENDIF (HAVE_GUILE)

ADD_CXXTEST(UnquoteUTest)
ADD_CXXTEST(LocalQuoteUTest)

IF (HAVE_GUILE)
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

	ADD_GUILE_TEST(DotProductTest dot-product-test.scm)
	ADD_GUILE_TEST(DotIdenticalTest dot-identical-test.scm)
	ADD_GUILE_TEST(DotChoiceTest dot-choice-test.scm)
	ADD_GUILE_TEST(DotLambdaTest dot-lambda-test.scm)
	ADD_GUILE_TEST(DotMashupTest dot-mashup-test.scm)
	ADD_GUILE_TEST(GroupByTest group-by-test.scm)
	ADD_GUILE_TEST(MeetLinkValueTest meet-link-value-test.scm)
	ADD_GUILE_TEST(MultiSpaceQueryTest multi-space-test.scm)
	ADD_GUILE_TEST(OrLinkTest or-link-test.scm)
	ADD_GUILE_TEST(OrMoreTest or-more-test.scm)
	ADD_GUILE_TEST(OrBindTest or-bind-test.scm)
	ADD_GUILE_TEST(OrEvalTest or-eval-test.scm)
	ADD_GUILE_TEST(QuoteStartTest quote-start-test.scm)
	ADD_GUILE_TEST(RecursiveTest recursive-test.scm)
	ADD_GUILE_TEST(SignatureTest signature-test.scm)
	ADD_GUILE_TEST(UnifyTest unify-test.scm)
	ADD_GUILE_TEST(MarginalsTest marginals-test.scm)
ENDIF (HAVE_GUILE)

# -------------------------------------------------------------
# Special unit-test chem atom types.
IF (HAVE_GUILE)
	OPENCOG_GEN_CXX_ATOMTYPES(chem_types.script
		chem_types.h
		chem_types.definitions
		chem_types.inheritance)

	OPENCOG_GEN_SCM_ATOMTYPES(chem_types.script chem_types.scm)

	ADD_CUSTOM_TARGET(chem_atom_types
		DEPENDS chem_types.h chem_types.scm)

	# The chem_types.h file is written to the build directory
	INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

	ADD_LIBRARY (chem-types SHARED
		chem_types.h
		ChemTypes.cc
	)

	# Without this, parallel make will race and crap up the generated files.
	ADD_DEPENDENCIES(chem-types chem_atom_types)

	TARGET_LINK_LIBRARIES(chem-types
		${ATOMSPACE_atomtypes_LIBRARY}
	)

	TARGET_LINK_LIBRARIES(SparseUTest chem-types)
ENDIF (HAVE_GUILE)

# -------------------------------------------------------------
# Run only the tests in this subdirectory
ADD_CUSTOM_TARGET (qtest
	DEPENDS tests
	WORKING_DIRECTORY .
	COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
	COMMENT "Running pattern-engine tests..."
)
