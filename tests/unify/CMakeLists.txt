LINK_LIBRARIES(
	unify
	atomspace
	clearbox
)
IF (HAVE_GUILE)
	LINK_LIBRARIES(smob)
ENDIF (HAVE_GUILE)

ADD_CXXTEST(UnifyUTest)
