ADD_LIBRARY (truthvalue
	AttentionValue.cc
	CountTruthValue.cc
	FuzzyTruthValue.cc
	GenericTruthValue.cc
	IndefiniteTruthValue.cc
	ProbabilisticTruthValue.cc
	SimpleTruthValue.cc
	EvidenceCountTruthValue.cc
	TruthValue.cc
)

TARGET_LINK_LIBRARIES(truthvalue
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS truthvalue
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AttentionValue.h
	CountTruthValue.h
	FuzzyTruthValue.h
	GenericTruthValue.h
	IndefiniteTruthValue.h
	ProbabilisticTruthValue.h
	SimpleTruthValue.h
	EvidenceCountTruthValue.h
	TruthValue.h
	DESTINATION "include/opencog/truthvalue"
)
