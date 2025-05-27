
IF (HAVE_GUILE)
	LINK_LIBRARIES(join smob execution atomspace)
	ADD_CXXTEST(JoinLinkUTest)
	ADD_CXXTEST(BlueJoinUTest)
	ADD_CXXTEST(TrueJoinUTest)
	ADD_CXXTEST(CompoundJoinUTest)
	ADD_CXXTEST(GPNJoinUTest)
ENDIF (HAVE_GUILE)
