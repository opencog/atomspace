
ADD_CXXTEST(CheckersUTest)
ADD_CXXTEST(FindUtilsUTest)
ADD_CXXTEST(NumberNodeUTest)

ADD_CXXTEST(DeleteLinkUTest)

IF(HAVE_GUILE)
	ADD_CXXTEST(FreeLinkUTest)
	ADD_CXXTEST(CondLinkUTest)

	ADD_GUILE_TEST(PresentAbsentTest present-absent-test.scm)
ENDIF(HAVE_GUILE)
