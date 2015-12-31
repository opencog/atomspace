
IF(HAVE_GUILE)
	ADD_CXXTEST(FreeLinkUTest)
	TARGET_LINK_LIBRARIES(FreeLinkUTest atomspace smob)
	ADD_CXXTEST(ParallelUTest)
	TARGET_LINK_LIBRARIES(ParallelUTest atomspace smob clearbox execution)
	ADD_CXXTEST(RandomUTest)
	TARGET_LINK_LIBRARIES(RandomUTest atomspace smob clearbox execution)
	ADD_CXXTEST(ReductUTest)
	TARGET_LINK_LIBRARIES(ReductUTest atomspace smob clearbox execution)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(AlphaConvertUTest)
TARGET_LINK_LIBRARIES(AlphaConvertUTest atomspace)

ADD_CXXTEST(DefineLinkUTest)
TARGET_LINK_LIBRARIES(DefineLinkUTest atomspace)

ADD_CXXTEST(DeleteLinkUTest)
TARGET_LINK_LIBRARIES(DeleteLinkUTest execution atomspace)

ADD_CXXTEST(PutLinkUTest)
TARGET_LINK_LIBRARIES(PutLinkUTest execution atomspace)

ADD_CXXTEST(StateLinkUTest)
TARGET_LINK_LIBRARIES(StateLinkUTest execution atomspace)
