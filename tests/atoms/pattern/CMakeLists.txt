LINK_LIBRARIES(
	atomcore
	atomspace
)

ADD_CXXTEST(BindLinkUTest)
TARGET_LINK_LIBRARIES(BindLinkUTest execution atomspace)
