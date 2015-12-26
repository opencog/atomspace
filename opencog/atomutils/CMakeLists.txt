
ADD_LIBRARY (atomutils SHARED
	AtomUtils
	FindUtils
	FuzzyMatch
)

ADD_DEPENDENCIES(atomutils opencog_atom_types)

TARGET_LINK_LIBRARIES(atomutils
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
	HandleMap.h
	Substitutor.h
	DESTINATION "include/opencog/atomutils"
)
