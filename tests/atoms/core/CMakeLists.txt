
LINK_LIBRARIES(
	atomcore
	atomspace
)

IF(HAVE_GUILE)
	ADD_CXXTEST(CondLinkUTest)
	TARGET_LINK_LIBRARIES(CondLinkUTest execution smob)

	ADD_CXXTEST(ScopeLinkUTest)
	TARGET_LINK_LIBRARIES(ScopeLinkUTest smob)

	ADD_CXXTEST(RewriteLinkUTest)
	TARGET_LINK_LIBRARIES(RewriteLinkUTest smob)

	ADD_CXXTEST(PutLinkUTest)
	TARGET_LINK_LIBRARIES(PutLinkUTest execution smob)

	ADD_CXXTEST(QuotationUTest)
	TARGET_LINK_LIBRARIES(QuotationUTest execution smob)

	ADD_GUILE_TEST(PutRecursiveUTest put-recursive-test.scm)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(CheckersUTest)
ADD_CXXTEST(FindUtilsUTest)
ADD_CXXTEST(NumberNodeUTest)
ADD_CXXTEST(TypeIntersectionUTest)
ADD_CXXTEST(TypeUtilsUTest)
ADD_CXXTEST(VariablesUTest)
