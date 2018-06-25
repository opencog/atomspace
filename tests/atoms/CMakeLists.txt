
IF(HAVE_GUILE)
	ADD_CXXTEST(FreeLinkUTest)
	TARGET_LINK_LIBRARIES(FreeLinkUTest smob atomspace)
	ADD_CXXTEST(MapLinkUTest)
	TARGET_LINK_LIBRARIES(MapLinkUTest execution smob atomspace)
	ADD_CXXTEST(ParallelUTest)
	TARGET_LINK_LIBRARIES(ParallelUTest clearbox execution smob atomspace)
	ADD_CXXTEST(RandomUTest)
	TARGET_LINK_LIBRARIES(RandomUTest clearbox execution smob atomspace)
	ADD_CXXTEST(ReductUTest)
	TARGET_LINK_LIBRARIES(ReductUTest clearbox execution smob atomspace)
	ADD_CXXTEST(RewriteLinkUTest)
	TARGET_LINK_LIBRARIES(RewriteLinkUTest smob atomspace)
	ADD_CXXTEST(ScopeLinkUTest)
	TARGET_LINK_LIBRARIES(ScopeLinkUTest smob atomspace)
	ADD_CXXTEST(PutLinkUTest)
	TARGET_LINK_LIBRARIES(PutLinkUTest execution smob atomspace)
	ADD_CXXTEST(QuotationUTest)
	TARGET_LINK_LIBRARIES(QuotationUTest execution smob atomspace)
	ADD_CXXTEST(HashUTest)
	TARGET_LINK_LIBRARIES(HashUTest smob atomspace)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(AlphaConvertUTest)
TARGET_LINK_LIBRARIES(AlphaConvertUTest query execution lambda atomspace)

ADD_CXXTEST(BetaReduceUTest)
TARGET_LINK_LIBRARIES(BetaReduceUTest atomspace)

ADD_CXXTEST(DefineLinkUTest)
TARGET_LINK_LIBRARIES(DefineLinkUTest atomspace)

ADD_CXXTEST(DeleteLinkUTest)
TARGET_LINK_LIBRARIES(DeleteLinkUTest execution atomspace)

ADD_CXXTEST(EqualLinkUTest)
TARGET_LINK_LIBRARIES(EqualLinkUTest execution atomspace)

ADD_CXXTEST(StateLinkUTest)
TARGET_LINK_LIBRARIES(StateLinkUTest execution atomspace)

ADD_CXXTEST(VariablesUTest)
TARGET_LINK_LIBRARIES(VariablesUTest atomspace)

ADD_CXXTEST(ExistsLinkUTest)
TARGET_LINK_LIBRARIES(ExistsLinkUTest execution atomspace)

ADD_CXXTEST(ValueOfUTest)
TARGET_LINK_LIBRARIES(ValueOfUTest clearbox execution atomspace)

ADD_CXXTEST(StreamUTest)
TARGET_LINK_LIBRARIES(StreamUTest clearbox execution smob atomspace)
