LINK_LIBRARIES(
	ure
	atomspace
	clearbox
)

ADD_CXXTEST(BackwardChainerUTest)
ADD_CXXTEST(ControlPolicyUTest)
ADD_CXXTEST(BITUTest)
ADD_CXXTEST(GradientUTest)
