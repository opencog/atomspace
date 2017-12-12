LINK_LIBRARIES(
	atomutils
	atomspace
	clearbox
)
IF (HAVE_GUILE)
	LINK_LIBRARIES(smob)
ENDIF (HAVE_GUILE)

ADD_CXXTEST(AtomUtilsUTest)
ADD_CXXTEST(FindUtilsUTest)
ADD_CXXTEST(FuzzyUTest)
ADD_CXXTEST(NeighborUTest)
