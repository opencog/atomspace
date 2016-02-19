ADD_LIBRARY (truthvalue
	AttentionValue.cc
	CountTruthValue.cc
	FuzzyTruthValue.cc
	GenericTruthValue.cc
	IndefiniteTruthValue.cc
	NullTruthValue.cc
	ProbabilisticTruthValue.cc
	SimpleTruthValue.cc
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
	NullTruthValue.h
	ProbabilisticTruthValue.h
	SimpleTruthValue.h
	TruthValue.h
	DESTINATION "include/opencog/truthvalue"
)
