ADD_LIBRARY (truthvalue_
	AttentionValue.cc
	CountTruthValue.cc
	EvidenceCountTruthValue.cc
	FuzzyTruthValue.cc
	IndefiniteTruthValue.cc
	ProbabilisticTruthValue.cc
	SimpleTruthValue.cc
	TruthValue.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(truthvalue_ opencog_atom_types)

TARGET_LINK_LIBRARIES(truthvalue_
	${COGUTIL_LIBRARY}
)

ADD_LIBRARY(truthvalue dummy)
TARGET_LINK_LIBRARIES(truthvalue
	truthvalue_
	atomspace
)

INSTALL (TARGETS truthvalue_
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (TARGETS truthvalue
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AttentionValue.h
	CountTruthValue.h
	FuzzyTruthValue.h
	IndefiniteTruthValue.h
	ProbabilisticTruthValue.h
	SimpleTruthValue.h
	EvidenceCountTruthValue.h
	TruthValue.h
	DESTINATION "include/opencog/truthvalue"
)
