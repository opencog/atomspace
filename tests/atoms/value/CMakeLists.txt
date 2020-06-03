LINK_LIBRARIES(
	value
	atombase
	exec
)

# Tests in order of increasing functional complexity/dependency
ADD_CXXTEST(ValueUTest)

ADD_CXXTEST(StreamUTest)
TARGET_LINK_LIBRARIES(StreamUTest smob atomspace)

# Test is missing from pull req #2667
# ADD_CXXTEST(VoidValueUTest)
