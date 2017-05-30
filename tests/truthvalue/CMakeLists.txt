
LINK_LIBRARIES(
	attentionbank
	atomspace
	atombase
	atomutils
)

ADD_CXXTEST(SimpleTruthValueUTest)
ADD_CXXTEST(EvidenceCountTruthValueUTest)
# ADD_CXXTEST(IndefiniteTruthValueUTest)
ADD_CXXTEST(TVMergeUTest)
ADD_CXXTEST(AttentionValueUTest)
