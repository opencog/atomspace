
# Build the query-engine library
ADD_LIBRARY(query-engine
	ContinuationMixin.cc
	InitiateSearchMixin.cc
	NextSearchMixin.cc
	PatternMatchEngine.cc
	Recognizer.cc
	RewriteMixin.cc
	Satisfier.cc
	SatisfyMixin.cc
	TermMatchMixin.cc
)

# Optionally enable debug logging for the pattern matcher.
TARGET_COMPILE_OPTIONS(query-engine PRIVATE -DQDEBUG=1)

# Optionally enable multi-threaded pattern matcher. Experimental.
# TARGET_COMPILE_OPTIONS(query-engine PRIVATE -DUSE_THREADED_PATTERN_ENGINE=1)

ADD_DEPENDENCIES(query-engine
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(query-engine
	execution
)

INSTALL (TARGETS query-engine
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	ContinuationMixin.h
	Implicator.h
	InitiateSearchMixin.h
	PatternMatchCallback.h
	PatternMatchEngine.h
	RewriteMixin.h
	Satisfier.h
	SatisfyMixin.h
	TermMatchMixin.h
	DESTINATION "include/opencog/query"
)
