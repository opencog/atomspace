
LINK_LIBRARIES(
	ruleengine
	atomspace
	clearbox
)

ADD_CXXTEST(ChainerUtilsUTest)
ADD_CXXTEST(ForwardChainerUTest)
ADD_CXXTEST(BackwardChainerUTest)
ADD_CXXTEST(URECommonsUTest)
ADD_CXXTEST(UREConfigReaderUTest)
