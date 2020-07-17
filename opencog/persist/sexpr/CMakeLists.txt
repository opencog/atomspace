
ADD_LIBRARY (sexpr
	fast_load
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

# For backwards compat, the file-specific code goes into it's own
# directory.
INSTALL (FILES
	fast_load.h
	DESTINATION "include/opencog/persist/file"
)

# Generic S-expression code.
INSTALL (FILES
	SexprDecode.h
	DESTINATION "include/opencog/persist/sexpr"
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
