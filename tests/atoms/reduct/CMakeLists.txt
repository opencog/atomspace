IF(HAVE_GUILE)
	ADD_CXXTEST(HeavisideUTest)
	TARGET_LINK_LIBRARIES(HeavisideUTest clearbox execution smob atomspace)

	ADD_CXXTEST(ReductUTest)
	TARGET_LINK_LIBRARIES(ReductUTest clearbox execution smob atomspace)
ENDIF(HAVE_GUILE)
