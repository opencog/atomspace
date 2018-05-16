
ADD_LIBRARY (atomutils_
	AtomUtils
	FindUtils
	FuzzyMatch
	FuzzyMatchBasic
	Neighbors
	TypeUtils
)

ADD_DEPENDENCIES(atomutils_ opencog_atom_types)

TARGET_LINK_LIBRARIES(atomutils_
	atombase
	${COGUTIL_LIBRARY}
)

ADD_LIBRARY (atomutils dummy)
TARGET_LINK_LIBRARIES(atomutils
	atomutils_
	lambda_
	atomspace
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
	ThreadSafeHandleMap.h
	Neighbors.h
	Substitutor.h
	TypeUtils.h
	DESTINATION "include/opencog/atomutils"
)
