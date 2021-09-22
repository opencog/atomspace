
ADD_LIBRARY (atomspaceutils
	AtomSpaceUtils
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
	DESTINATION "include/opencog/atomspaceutils"
)
