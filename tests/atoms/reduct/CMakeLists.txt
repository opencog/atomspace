IF(HAVE_GUILE)
	LINK_LIBRARIES(clearbox execution smob atomspace)

	ADD_CXXTEST(ReductUTest)
	ADD_CXXTEST(HeavisideUTest)
	ADD_CXXTEST(MinMaxUTest)
	ADD_CXXTEST(AccumulateUTest)
ENDIF(HAVE_GUILE)
