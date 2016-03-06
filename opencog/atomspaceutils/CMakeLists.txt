
ADD_LIBRARY (atomspaceutils
	AtomSpaceUtils
	RandomAtomGenerator
)

ADD_DEPENDENCIES(atomspaceutils opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspaceutils
	atombase
	atomspace
	${COGUTIL_LIBRARY}
)

IF (CYGWIN)
	INSTALL (TARGETS atomspaceutils
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ELSE (CYGWIN)
	INSTALL (TARGETS atomspaceutils
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (CYGWIN)

INSTALL (FILES
	AtomSpaceUtils.h
	DESTINATION "include/opencog/atomspaceutils"
)
