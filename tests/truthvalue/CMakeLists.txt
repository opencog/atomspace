
LINK_LIBRARIES(
	atomspace
	value
	distvalue
	atomutils
)

ADD_CXXTEST(SimpleTruthValueUTest)
ADD_CXXTEST(EvidenceCountTruthValueUTest)
# ADD_CXXTEST(IndefiniteTruthValueUTest)
ADD_CXXTEST(TVMergeUTest)
ADD_CXXTEST(DVTest)
ADD_CXXTEST(CDVTest)
ADD_CXXTEST(DVFormulaTest)
