
ADD_CXXTEST(DefineLinkUTest)
ADD_CXXTEST(StateLinkUTest)

IF(HAVE_GUILE)
	ADD_GUILE_TEST(GrantTest grant-test.scm)
ENDIF(HAVE_GUILE)
