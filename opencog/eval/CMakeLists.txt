
ADD_LIBRARY (generic-eval
	GenericASEval.cc
)

TARGET_LINK_LIBRARIES(generic-eval
	atomspace
)

INSTALL (TARGETS generic-eval
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
   GenericEval.h
   GenericASEval.h
   DESTINATION "include/opencog/eval"
)
