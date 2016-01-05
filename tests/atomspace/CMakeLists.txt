
LINK_LIBRARIES(
	atombase
	atomutils
	atomspace
	clearbox
)

IF(HAVE_GUILE)
	ADD_CXXTEST(AtomTableUTest)
	TARGET_LINK_LIBRARIES(AtomTableUTest smob clearbox)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(ClassServerUTest)
ADD_CXXTEST(AtomUTest)
ADD_CXXTEST(NodeUTest)
ADD_CXXTEST(LinkUTest)
ADD_CXXTEST(SimpleTruthValueUTest)
# ADD_CXXTEST(IndefiniteTruthValueUTest)
ADD_CXXTEST(TVMergeUTest)
ADD_CXXTEST(AttentionValueUTest)
ADD_CXXTEST(TLBUTest)
ADD_CXXTEST(AtomSpaceUTest)
ADD_CXXTEST(AtomSpaceImplUTest)
ADD_CXXTEST(AtomSpaceAsyncUTest)
ADD_CXXTEST(UseCountUTest)
ADD_CXXTEST(MultiSpaceUTest)
ADD_CXXTEST(RemoveUTest)
ADD_CXXTEST(HandleMapUTest)
