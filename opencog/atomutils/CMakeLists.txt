
ADD_LIBRARY (atomutils
	AtomUtils
	FuzzyMatch
	FuzzyMatchBasic
	Neighbors
)

ADD_DEPENDENCIES(atomutils opencog_atom_types)

TARGET_LINK_LIBRARIES(atomutils
	atombase
	${COGUTIL_LIBRARY}
)

IF (CYGWIN)
    INSTALL (TARGETS atomutils EXPORT AtomSpaceTargets
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ELSE (CYGWIN)
    INSTALL (TARGETS atomutils EXPORT AtomSpaceTargets
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (CYGWIN)

INSTALL (FILES
	AtomUtils.h
	FollowLink.h
	ForeachChaseLink.h
	FuzzyMatch.h
	FuzzyMatchBasic.h
	ThreadSafeHandleMap.h
	Neighbors.h
	Substitutor.h
	DESTINATION "include/opencog/atomutils"
)
