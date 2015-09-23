IF (HAVE_GEARMAN)
IF (HAVE_GUILE)
	ADD_LIBRARY(dist-gearman SHARED
	    Distributed/DistSCM.cc
	)
	ADD_DEPENDENCIES(dist-gearman
		opencog_atom_types
	)
	TARGET_LINK_LIBRARIES(dist-gearman
	    ${GEARMAN_LIBRARY}
	    atomspace
	)
	TARGET_LINK_LIBRARIES(dist-gearman
		smob
	)
	INSTALL (TARGETS dist-gearman DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")	
	INSTALL (FILES
		Distributed/DistSCM.h
		DESTINATION "include/opencog/query"
	)
ELSE(HAVE_GUILE)
	MESSAGE(FATAL_ERROR "Cannot find guile")
ENDIF (HAVE_GUILE)

ELSE(HAVE_GEARMAN)
	MESSAGE(FATAL_ERROR "Cannot find libgearman library")
ENDIF(HAVE_GEARMAN)

# Build the query shlib
ADD_LIBRARY(query SHARED
	AttentionalFocusCB.cc
	DefaultPatternMatchCB.cc
	Implicator.cc
	InitiateSearchCB.cc
	PatternMatch.cc
	PatternMatchEngine.cc
	PatternSCM.cc
	Recognizer.cc
	Satisfier.cc
	FuzzyMatch/FuzzyPatternMatch.cc
	FuzzyMatch/FuzzyPatternMatchCB.cc
#	Distributed/DistSCM.cc
)
ADD_DEPENDENCIES(query
	opencog_atom_types
)
TARGET_LINK_LIBRARIES(query
	atomutils
	lambda
	atomspace
#	execution
#    gearman
)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(query
		smob
	)
ENDIF (HAVE_GUILE)


INSTALL (TARGETS query DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	AttentionalFocusCB.h
	BindLinkAPI.h
	DefaultImplicator.h
	DefaultPatternMatchCB.h
	Implicator.h
	InitiateSearchCB.h
	Pattern.h
	PatternTerm.h
	PatternSCM.h
	PatternMatchCallback.h
	PatternMatchEngine.h
	Satisfier.h
	DESTINATION "include/opencog/query"
)
