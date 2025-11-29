
ADD_CXXTEST(VariablesUTest)

IF(HAVE_GUILE)
	ADD_CXXTEST(ScopeLinkUTest)
	ADD_CXXTEST(RewriteLinkUTest)
	ADD_CXXTEST(PutLinkUTest)
	ADD_CXXTEST(QuotationUTest)

	ADD_GUILE_TEST(PutRecursiveTest put-recursive-test.scm)
ENDIF(HAVE_GUILE)
