#
# Rule engine lib build
#
ADD_LIBRARY(ruleengine
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

ADD_DEPENDENCIES(ruleengine
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(ruleengine
	lambda
	query-engine
	execution
	atomspace
	atomspaceutils
	unify
)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(ruleengine smob)
ENDIF (HAVE_GUILE)

INSTALL (TARGETS ruleengine 
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
	DESTINATION "include/opencog/rule-engine"
)

ADD_SUBDIRECTORY(forwardchainer)
ADD_SUBDIRECTORY(backwardchainer)
