
LINK_LIBRARIES(
	smob
	atomspace
	value
)

ADD_CXXTEST(SimpleTruthValueUTest)
ADD_CXXTEST(TVandAVUTest)
ADD_CXXTEST(TVMergeUTest)
ADD_CXXTEST(EvidenceCountTruthValueUTest)
# ADD_CXXTEST(IndefiniteTruthValueUTest)

# Configure LD_LIBRARY_PATH so that the unit test can be run before
# running "make install".
SET_PROPERTY(TEST TVandAVUTest
   PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/opencog/attentionbank")
