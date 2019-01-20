
LINK_LIBRARIES(
	smob
	atomspace
	value
	atomutils
)

ADD_CXXTEST(SimpleTruthValueUTest)
ADD_CXXTEST(TVandAVUTest)
ADD_CXXTEST(TVMergeUTest)
ADD_CXXTEST(EvidenceCountTruthValueUTest)
# ADD_CXXTEST(IndefiniteTruthValueUTest)
