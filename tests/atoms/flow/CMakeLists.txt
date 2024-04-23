LINK_LIBRARIES(
	atomflow
	atomcore
	atomspace
)

ADD_CXXTEST(ValueOfUTest)
ADD_CXXTEST(StreamValueOfUTest)

ADD_GUILE_TEST(CollectionOfTest collection-of-test.scm)
ADD_GUILE_TEST(IncomingOfTest incoming-of-test.scm)

ADD_CXXTEST(FormulaUTest)
TARGET_LINK_LIBRARIES(FormulaUTest execution smob)

ADD_CXXTEST(SetTVUTest)
TARGET_LINK_LIBRARIES(SetTVUTest execution smob)

ADD_CXXTEST(SetValueUTest)
TARGET_LINK_LIBRARIES(SetValueUTest execution smob)

ADD_CXXTEST(DynamicUTest)
TARGET_LINK_LIBRARIES(DynamicUTest execution smob)

ADD_CXXTEST(FilterLinkUTest)
TARGET_LINK_LIBRARIES(FilterLinkUTest execution smob)

ADD_GUILE_TEST(FilterGlobTest filter-glob-test.scm)
ADD_GUILE_TEST(FilterValueTest filter-value-test.scm)
ADD_GUILE_TEST(FilterFloatTest filter-float-test.scm)
