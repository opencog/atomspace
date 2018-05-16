
# Build the query shlib
ADD_LIBRARY(query_
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

ADD_DEPENDENCIES(query_
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(query_
	lambda_
)

ADD_LIBRARY(query dummy)
TARGET_LINK_LIBRARIES(query
	query_
	lambda
	execution_
	smob
	atomspace
)

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
