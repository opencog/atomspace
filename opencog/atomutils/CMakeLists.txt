
ADD_LIBRARY (atomutils
	AtomUtils
	FindUtils
	FuzzyMatch
	FuzzyMatchBasic
	Neighbors
	TypeUtils
)

ADD_DEPENDENCIES(atomutils opencog_atom_types)

TARGET_LINK_LIBRARIES(atomutils
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

IF (CYGWIN)
	INSTALL (TARGETS atomutils
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ELSE (CYGWIN)
	INSTALL (TARGETS atomutils
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (CYGWIN)

INSTALL (FILES
	AtomUtils.h
	FindUtils.h
	FollowLink.h
	ForeachChaseLink.h
	FuzzyMatch.h
	FuzzyMatchBasic.h
	HandleMap.h
	Neighbors.h
	Substitutor.h
	TypeUtils.h
	DESTINATION "include/opencog/atomutils"
)
