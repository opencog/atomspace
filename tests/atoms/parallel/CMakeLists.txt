
IF(HAVE_GUILE)
	ADD_CXXTEST(ParallelUTest)
	TARGET_LINK_LIBRARIES(ParallelUTest parallel clearbox execution smob atomspace)
	ADD_CXXTEST(ThreadedUTest)
	TARGET_LINK_LIBRARIES(ThreadedUTest parallel clearbox execution smob atomspace)
ENDIF(HAVE_GUILE)
