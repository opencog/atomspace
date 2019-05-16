ADD_LIBRARY (distvalue
	DVecUtils.cc
	CoverTree.cc
	CTHist.cc
)

TARGET_LINK_LIBRARIES(distvalue
	${COGUTIL_LIBRARY}
)

INSTALL (FILES
	DVecUtils.h
	CoverTree.h
	CTHist.h
	DESTINATION "include/opencog/atoms/distvalue"
)
