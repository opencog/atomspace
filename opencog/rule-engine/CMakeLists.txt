#
# Rule engine lib build
#
ADD_LIBRARY(ruleengine SHARED
	backwardchainer/BackwardChainer.cc
	backwardchainer/BackwardChainerPMCB.cc
	backwardchainer/Target.cc
	backwardchainer/UnifyPMCB.cc
	forwardchainer/FCStat.cc
	forwardchainer/FocusSetPMCB.h
	forwardchainer/ForwardChainer.cc
	forwardchainer/VarGroundingPMCB.h
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
	smob
	execution
	atomutils
	atomspace
	atomspaceutils
)

INSTALL (TARGETS ruleengine DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	InferenceSCM.h
	UREConfigReader.h
	URECommons.h
	Rule.h
	UREConfigReader.h
	DESTINATION "include/opencog/rule-engine"
)

ADD_SUBDIRECTORY(forwardchainer)
ADD_SUBDIRECTORY(backwardchainer)
