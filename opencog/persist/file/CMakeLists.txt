
ADD_LIBRARY (load_scm
	fast_load
	AtomSexpr
	ValueSexpr
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
	DESTINATION "include/opencog/persist/file"
)

INSTALL (FILES
	SexprDecode.h
	DESTINATION "include/opencog/persist/sexpr"
)

# -------------------------------

ADD_LIBRARY (persist-file
	PersistFileSCM.cc
)

TARGET_LINK_LIBRARIES(persist-file
	load_scm
	atomspace
	smob
)

ADD_GUILE_EXTENSION(SCM_CONFIG persist-file "opencog-ext-path-persist-file")

INSTALL (TARGETS persist-file EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
