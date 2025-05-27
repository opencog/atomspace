
LINK_LIBRARIES(
	atomcore
	atomspace
)

ADD_CXXTEST(CheckersUTest)
ADD_CXXTEST(FindUtilsUTest)
ADD_CXXTEST(NumberNodeUTest)
ADD_CXXTEST(TypeIntersectionUTest)
ADD_CXXTEST(TypeUtilsUTest)
ADD_CXXTEST(VariablesUTest)

ADD_CXXTEST(DefineLinkUTest)
ADD_CXXTEST(DeleteLinkUTest)
ADD_CXXTEST(StateLinkUTest)

IF(HAVE_GUILE)
	LINK_LIBRARIES(execution smob)

	ADD_CXXTEST(FreeLinkUTest)
	ADD_CXXTEST(CondLinkUTest)
	ADD_CXXTEST(ScopeLinkUTest)
	ADD_CXXTEST(RewriteLinkUTest)
	ADD_CXXTEST(PutLinkUTest)
	ADD_CXXTEST(QuotationUTest)

	ADD_GUILE_TEST(GrantTest grant-test.scm)
	ADD_GUILE_TEST(PutRecursiveTest put-recursive-test.scm)
	ADD_GUILE_TEST(PresentTest present-test.scm)
ENDIF(HAVE_GUILE)
