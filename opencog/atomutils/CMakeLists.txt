
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

INSTALL (TARGETS atomutils_ DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (TARGETS atomutils DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

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
