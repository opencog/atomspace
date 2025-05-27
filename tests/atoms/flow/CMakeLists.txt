LINK_LIBRARIES(
	atomflow
	atomcore
	atomspace
)

ADD_CXXTEST(ValueOfUTest)
ADD_CXXTEST(StreamValueOfUTest)

IF (HAVE_GUILE)
	ADD_GUILE_TEST(CollectionOfTest collection-of-test.scm)
	ADD_GUILE_TEST(IncomingOfTest incoming-of-test.scm)

	LINK_LIBRARIES(execution smob)
	ADD_CXXTEST(FormulaUTest)
	ADD_CXXTEST(SetTVUTest)
	ADD_CXXTEST(SetValueUTest)
	ADD_CXXTEST(DynamicUTest)
	ADD_CXXTEST(FilterLinkUTest)

	ADD_GUILE_TEST(IncrementValueTest increment-value-test.scm)
	ADD_GUILE_TEST(FilterGlobTest filter-glob-test.scm)
	ADD_GUILE_TEST(FilterValueTest filter-value-test.scm)
	ADD_GUILE_TEST(FilterFloatTest filter-float-test.scm)
	ADD_GUILE_TEST(FilterRuleTest filter-rule-test.scm)
	ADD_GUILE_TEST(StringOfTest string-of-test.scm)
	ADD_GUILE_TEST(FilterStringsTest filter-strings-test.scm)
	ADD_GUILE_TEST(ConcatenateTest concatenate-test.scm)
	ADD_GUILE_TEST(SplitTest split-test.scm)
ENDIF (HAVE_GUILE)
