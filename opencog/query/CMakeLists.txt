
# Build the query shlib
ADD_LIBRARY(query SHARED
	AttentionalFocusCB.cc
	Composition.cc
	DefaultPatternMatchCB.cc
	Implicator.cc
	Instantiator.cc
	PatternMatch.cc
	PatternMatchEngine.cc
	PatternSCM.cc
	Satisfier.cc
	FuzzyMatch/FuzzyPatternMatch.cc
	FuzzyMatch/FuzzyPatternMatchCB.cc
)

ADD_DEPENDENCIES(query
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(query
	atomutils
	lambda
	atomspace
	execution
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(query
		smob
	)
ENDIF (HAVE_GUILE)


INSTALL (TARGETS query DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	AttentionalFocusCB.h
	BindLink.h
	DefaultImplicator.h
	DefaultPatternMatchCB.h
	Implicator.h
	Instantiator.h
	Pattern.h
	PatternSCM.h
	PatternMatchCallback.h
	PatternMatchEngine.h
	Satisfier.h
	DESTINATION "include/${PROJECT_NAME}/query"
)
