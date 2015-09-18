
# Build the query shlib
ADD_LIBRARY(query SHARED
	AttentionalFocusCB.cc
	DefaultPatternMatchCB.cc
	Implicator.cc
	InitiateSearchCB.cc
	PatternMatch.cc
	PatternMatchEngine.cc
	PatternSCM.cc
	Satisfier.cc
	FuzzyMatch/FuzzyPatternMatch.cc
	FuzzyMatch/FuzzyPatternMatchCB.cc
#	Distributed/DistSCM.cc
)
ADD_LIBRARY(dist-gearman SHARED
    Distributed/DistSCM.cc
)
ADD_DEPENDENCIES(query
	opencog_atom_types
)
ADD_DEPENDENCIES(dist-gearman
	opencog_atom_types
)
TARGET_LINK_LIBRARIES(query
	atomutils
	lambda
	atomspace
#	execution
#    gearman
)
TARGET_LINK_LIBRARIES(dist-gearman
    gearman
    atomspace
#    persist-sql
)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(query
		smob
	)
	TARGET_LINK_LIBRARIES(dist-gearman
		smob
	)	
ENDIF (HAVE_GUILE)


INSTALL (TARGETS query DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (TARGETS dist-gearman DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
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
	Distributed/DistSCM.h
	DESTINATION "include/opencog/query"
)
