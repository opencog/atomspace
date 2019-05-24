ADD_LIBRARY (distvalue
	DVecUtils.cc
	CoverTree.cc
	CTHist.cc
	DistributionalValue.cc
	ConditionalDV.cc
	DVFormulas.cc
	DistributionalValueSCM.cc
)

TARGET_LINK_LIBRARIES(distvalue
	smob
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS distvalue EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	DVecUtils.h
	CoverTree.h
	CTHist.h
	DistributionalValue.h
	ConditionalDV.h
	DVFormulas.h
	DistributionalValueSCM.h
	DESTINATION "include/opencog/atoms/distvalue"
)
