
ADD_LIBRARY (load_scm
        fast_load
)

ADD_DEPENDENCIES(load_scm opencog_atom_types)

TARGET_LINK_LIBRARIES(load_scm
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS load_scm EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
        fast_load.h
	DESTINATION "include/opencog/persist/load_scm"
)
