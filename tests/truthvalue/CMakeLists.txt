
LINK_LIBRARIES(
	atomspace
	atomproto
	atomutils
)

ADD_CXXTEST(SimpleTruthValueUTest)
ADD_CXXTEST(EvidenceCountTruthValueUTest)
# ADD_CXXTEST(IndefiniteTruthValueUTest)
ADD_CXXTEST(TVMergeUTest)
