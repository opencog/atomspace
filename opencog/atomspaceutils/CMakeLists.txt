
ADD_LIBRARY (atomspaceutils
	AtomSpaceUtils
	TLB
)

ADD_DEPENDENCIES(atomspaceutils opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspaceutils
	atombase
	atomspace
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomspaceutils EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AtomSpaceUtils.h
	TLB.h
	DESTINATION "include/opencog/atomspaceutils"
)
