#
# Rule engine lib build
#
ADD_LIBRARY(ruleengine
	backwardchainer/BackwardChainer
	backwardchainer/TraceRecorder
	backwardchainer/BackwardChainerPMCB
	backwardchainer/BIT
	backwardchainer/Fitness
	forwardchainer/FCStat
	forwardchainer/ForwardChainer
	URELogger
	ChainerUtils.cc
	InferenceSCM.cc
	Rule.cc
	URECommons.cc
	UREConfigReader.cc
)

ADD_DEPENDENCIES(ruleengine
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(ruleengine
	lambda
	query
	execution
	atomutils
	atomspace
	atomspaceutils
)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(ruleengine smob)
ENDIF (HAVE_GUILE)

INSTALL (TARGETS ruleengine DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	UREConfigReader.h
	URECommons.h
    URELogger.h
	Rule.h
	UREConfigReader.h
	DESTINATION "include/opencog/rule-engine"
)

ADD_SUBDIRECTORY(forwardchainer)
ADD_SUBDIRECTORY(backwardchainer)
