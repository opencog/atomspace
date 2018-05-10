
# Build the query shlib
ADD_LIBRARY(query
	DefaultPatternMatchCB.cc
	Implicator.cc
	DefaultImplicator.cc
	InitiateSearchCB.cc
	PatternMatch.cc
	PatternMatchEngine.cc
	PatternSCM.cc
	Recognizer.cc
	Satisfier.cc
)

ADD_DEPENDENCIES(query
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(query
	atomutils
	lambda
	atomspace
#	execution # disabled to avoid cyclic dependency
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(query smob)
ENDIF (HAVE_GUILE)

INSTALL (TARGETS query DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	BindLinkAPI.h
	DefaultImplicator.h
	DefaultPatternMatchCB.h
	Implicator.h
	InitiateSearchCB.h
	PatternMatchCallback.h
	PatternMatchEngine.h
	Satisfier.h
	DESTINATION "include/opencog/query"
)
