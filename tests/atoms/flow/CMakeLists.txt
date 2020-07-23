LINK_LIBRARIES(
	atomflow
	atomcore
	atomspace
)

ADD_CXXTEST(ValueOfUTest)
ADD_CXXTEST(StreamValueOfUTest)

ADD_CXXTEST(FormulaUTest)
TARGET_LINK_LIBRARIES(FormulaUTest execution smob)

ADD_CXXTEST(SetTVUTest)
TARGET_LINK_LIBRARIES(SetTVUTest execution smob)

ADD_CXXTEST(SetValueUTest)
TARGET_LINK_LIBRARIES(SetValueUTest execution smob)

ADD_CXXTEST(DynamicUTest)
TARGET_LINK_LIBRARIES(DynamicUTest execution smob)
