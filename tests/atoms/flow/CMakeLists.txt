LINK_LIBRARIES(
	atomflow
	atomcore
	atomspace
)

ADD_CXXTEST(ValueOfUTest)

ADD_CXXTEST(SetTVUTest)
TARGET_LINK_LIBRARIES(SetTVUTest execution smob)
