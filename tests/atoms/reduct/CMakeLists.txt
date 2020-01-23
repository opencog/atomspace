IF(HAVE_GUILE)
	ADD_CXXTEST(ReductUTest)
	TARGET_LINK_LIBRARIES(ReductUTest clearbox execution smob atomspace)
ENDIF(HAVE_GUILE)
