
# Tests in order of increasing functional complexity/dependency
ADD_CXXTEST(ValueUTest)
ADD_CXXTEST(SortedValueUTest)
ADD_CXXTEST(GroupValueUTest)

IF (HAVE_GUILE)
	ADD_CXXTEST(StreamUTest)

	ADD_GUILE_TEST(BoolValuePrintTest bool-value-print-test.scm)
	ADD_GUILE_TEST(CtorArgsTest ctor-args-test.scm)
	ADD_GUILE_TEST(FlatStreamTest flat-stream-test.scm)
	ADD_GUILE_TEST(SortingTest sorting-test.scm)
ENDIF (HAVE_GUILE)

