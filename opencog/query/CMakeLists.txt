
# Build the query-engine library
ADD_LIBRARY(query-engine
	DefaultPatternMatchCB.cc
	Implicator.cc
	DefaultImplicator.cc
	InitiateSearchCB.cc
	PatternMatch.cc
	PatternMatchEngine.cc
	Recognizer.cc
	Satisfier.cc
)

ADD_DEPENDENCIES(query-engine
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(query-engine
	atomspace
	execution
)

ADD_LIBRARY(query
	PatternSCM.cc
)

TARGET_LINK_LIBRARIES(query
	query-engine
	smob
)

INSTALL (TARGETS query-engine
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (TARGETS query
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

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
