#
# Rule engine lib build
#
ADD_LIBRARY(ure
	backwardchainer/BackwardChainer
	backwardchainer/TraceRecorder
	backwardchainer/ControlPolicy
	backwardchainer/BIT
	backwardchainer/Fitness
	forwardchainer/FCStat
	forwardchainer/ForwardChainer
	forwardchainer/SourceSet
	URELogger
	InferenceSCM
	Rule
	UREConfig
	MixtureModel
	ActionSelection
	BetaDistribution
	ThompsonSampling
)

ADD_DEPENDENCIES(ure
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(ure
	lambda
	query-engine
	execution
	atomspace
	atomspaceutils
	unify
)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(ure smob)
	ADD_GUILE_EXTENSION(SCM_CONFIG ure "opencog-ext-path-ure")
ENDIF (HAVE_GUILE)

INSTALL (TARGETS ure 
    EXPORT AtomSpaceTargets 
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	UREConfig.h
	URELogger.h
	Rule.h
	UREConfig.h
	MixtureModel.h
	ActionSelection.h
	BetaDistribution.h
	ThompsonSampling.h
	DESTINATION "include/opencog/ure"
)

ADD_SUBDIRECTORY(forwardchainer)
ADD_SUBDIRECTORY(backwardchainer)
