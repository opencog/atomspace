
ADD_CXXTEST(AssignUTest)
TARGET_LINK_LIBRARIES(AssignUTest atomspace clearbox)

IF(HAVE_GUILE)
	ADD_CXXTEST(ReductUTest)
	TARGET_LINK_LIBRARIES(ReductUTest atomspace smob clearbox execution)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(DefineLinkUTest)
TARGET_LINK_LIBRARIES(DefineLinkUTest atomspace)
