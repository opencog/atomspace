
LINK_LIBRARIES(
	atomcore
	atomspace
)

ADD_CXXTEST(CheckersUTest)
ADD_CXXTEST(CondLinkUTest)
TARGET_LINK_LIBRARIES(CondLinkUTest execution smob atomspace)
ADD_CXXTEST(FindUtilsUTest)
