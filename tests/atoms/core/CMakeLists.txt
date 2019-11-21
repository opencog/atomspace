
LINK_LIBRARIES(
	atomcore
	atomspace
)

IF(HAVE_GUILE)
	ADD_CXXTEST(CondLinkUTest)
	TARGET_LINK_LIBRARIES(CondLinkUTest execution smob atomspace)

	ADD_CXXTEST(ScopeLinkUTest)
	TARGET_LINK_LIBRARIES(ScopeLinkUTest smob atomspace)

	ADD_CXXTEST(RewriteLinkUTest)
	TARGET_LINK_LIBRARIES(RewriteLinkUTest smob atomspace)

	ADD_CXXTEST(PutLinkUTest)
	TARGET_LINK_LIBRARIES(PutLinkUTest execution smob atomspace)

	ADD_CXXTEST(QuotationUTest)
	TARGET_LINK_LIBRARIES(QuotationUTest execution smob atomspace)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(CheckersUTest)

ADD_CXXTEST(FindUtilsUTest)

ADD_CXXTEST(TypeUtilsUTest)

ADD_CXXTEST(VariablesUTest)
TARGET_LINK_LIBRARIES(VariablesUTest atomspace)
