ADD_LIBRARY (truthvalue
	AttentionValue.cc
	CountTruthValue.cc
	EvidenceCountTruthValue.cc
	FuzzyTruthValue.cc
	IndefiniteTruthValue.cc
	ProbabilisticTruthValue.cc
	SimpleTruthValue.cc
	TruthValue.cc
    DistributionalValue.cc
    ConditionalDV.cc
	DVFormulas.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(truthvalue opencog_atom_types)

TARGET_LINK_LIBRARIES(truthvalue
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS truthvalue EXPORT AtomSpaceTargets
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
    DistributionalValue.h
    ConditionalDV.h
	DVFormulas.h
	DESTINATION "include/opencog/truthvalue"
)
