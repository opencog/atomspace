LINK_LIBRARIES(
	atomspace
	value
	atombase
)

# Tests in order of increasing functional complexity/dependency
ADD_CXXTEST(ValueUTest)
ADD_CXXTEST(VoidValueUTest)

IF (HAVE_GUILE)
	ADD_CXXTEST(StreamUTest)
	TARGET_LINK_LIBRARIES(StreamUTest smob)
ENDIF (HAVE_GUILE)

