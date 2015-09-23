FIND_PATH(GEARMAN_INCLUDE_DIR NAMES gearman.h 
	PATHS /usr/include/libgearman /usr/local/include/libgearman)

FIND_LIBRARY(GEARMAN_LIBRARY NAMES gearman PATHS /usr/lib /usr/local/lib)

IF (GEARMAN_INCLUDE_DIR)
IF (HAVE_GUILE)

	MESSAGE(STATUS "gearman Include dir: ${GEARMAN_INCLUDE_DIR}")
	MESSAGE(STATUS "libgearman library: ${GEARMAN_LIBRARY}")
	ADD_LIBRARY(dist-gearman SHARED
	    Distributed/DistSCM.cc
	)
	ADD_DEPENDENCIES(dist-gearman
		opencog_atom_types
	)
	TARGET_LINK_LIBRARIES(dist-gearman
	    gearman
	    atomspace
	)
	TARGET_LINK_LIBRARIES(dist-gearman
		smob
	)
	INSTALL (TARGETS dist-gearman DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")	
ELSE()
	MESSAGE(FATAL_ERROR "Cannot find guile")
ENDIF (HAVE_GUILE)

ELSE()
	MESSAGE(FATAL_ERROR "Cannot find libgearman library")
ENDIF()

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
	Distributed/DistSCM.h
	DESTINATION "include/opencog/query"
)
