
IF(HAVE_GUILE)
	ADD_CXXTEST(ParallelUTest)
	TARGET_LINK_LIBRARIES(ParallelUTest clearbox execution smob atomspace)
ENDIF(HAVE_GUILE)
