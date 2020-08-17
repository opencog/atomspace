
IF(HAVE_GUILE)
	ADD_CXXTEST(ParallelUTest)
	TARGET_LINK_LIBRARIES(ParallelUTest parallel clearbox execution smob atomspace)
ENDIF(HAVE_GUILE)
