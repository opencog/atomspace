LINK_LIBRARIES(
	value
)

# Tests in order of increasing functional complexity/dependency
ADD_CXXTEST(ValueUTest)

ADD_CXXTEST(StreamUTest)
TARGET_LINK_LIBRARIES(StreamUTest smob atomspace)
