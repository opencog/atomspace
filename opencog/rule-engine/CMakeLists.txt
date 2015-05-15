#
# Rule engine lib build
#
ADD_LIBRARY(ruleengine SHARED
	backwardchainer/BackwardChainer.cc
	backwardchainer/BackwardChainerPMCB.cc
	URECommons.cc
	forwardchainer/ForwardChainer.cc
	forwardchainer/ForwardChainInputMatchCB.cc
	forwardchainer/ForwardChainPatternMatchCB.cc
	forwardchainer/ForwardChainerCallBack.h
	forwardchainer/DefaultForwardChainerCB.cc
	forwardchainer/FCMemory.cc	
	InferenceSCM.cc
	Rule.cc
	JsonicControlPolicyParamLoader.cc
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
)

INSTALL (TARGETS ruleengine DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	InferenceSCM.h
	JsonicControlPolicyParamLoader.h
	URECommons.h
	Rule.h
	DESTINATION "include/${PROJECT_NAME}/rule-engine"
)

ADD_SUBDIRECTORY(forwardchainer)
ADD_SUBDIRECTORY(backwardchainer)
