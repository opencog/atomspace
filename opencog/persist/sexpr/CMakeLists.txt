
# Generic S-expression decoding.
ADD_LIBRARY (sexpr
	AtomSexpr
	ValueSexpr
)

ADD_DEPENDENCIES(sexpr opencog_atom_types)

TARGET_LINK_LIBRARIES(sexpr
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS sexpr EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Sexpr.h
	DESTINATION "include/opencog/persist/sexpr"
)

# -------------------------------
# File-specific code.

ADD_LIBRARY (load_scm
	fast_load
)

TARGET_LINK_LIBRARIES(load_scm
	sexpr
)

INSTALL (TARGETS load_scm EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	fast_load.h
	DESTINATION "include/opencog/persist/file"
)

# -------------------------------

ADD_LIBRARY (persist-file
	PersistFileSCM.cc
)

TARGET_LINK_LIBRARIES(persist-file
	sexpr
	atomspace
	smob
)

ADD_GUILE_EXTENSION(SCM_CONFIG persist-file "opencog-ext-path-persist-file")

INSTALL (TARGETS persist-file EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
