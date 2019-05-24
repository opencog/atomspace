ADD_LIBRARY (distvalue
	DVecUtils.cc
	CoverTree.cc
	CTHist.cc
	DistributionalValue.cc
	ConditionalDV.cc
	DVFormulas.cc
)

TARGET_LINK_LIBRARIES(distvalue
	${COGUTIL_LIBRARY}
)

INSTALL (FILES
	DVecUtils.h
	CoverTree.h
	CTHist.h
	DistributionalValue.h
	ConditionalDV.h
	DVFormulas.h
	DESTINATION "include/opencog/atoms/distvalue"
)
