LINK_LIBRARIES(
	ruleengine
	atomspace
	clearbox
)

ADD_CXXTEST(BetaDistributionUTest)
ADD_CXXTEST(BackwardChainerUTest)
ADD_CXXTEST(ActionSelectionUTest)
ADD_CXXTEST(ControlPolicyUTest)
ADD_CXXTEST(BITUTest)
