LINK_LIBRARIES(
	smob
	value
	atomspace
)

# Tests in order of increasing functional complexity/dependency
ADD_CXXTEST(ValueUTest)
ADD_CXXTEST(StreamUTest)
