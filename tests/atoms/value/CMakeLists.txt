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

	# Temp disable while under construction.
	# ADD_CXXTEST(SortedStreamUTest)
	# TARGET_LINK_LIBRARIES(SortedStreamUTest smob)

	ADD_GUILE_TEST(BoolValuePrintTest bool-value-print-test.scm)
	ADD_GUILE_TEST(CtorArgsTest ctor-args-test.scm)
	ADD_GUILE_TEST(FlatStreamTest flat-stream-test.scm)
ENDIF (HAVE_GUILE)

