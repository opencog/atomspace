
# Generic S-expression decoding.
ADD_LIBRARY (sexpr
	AtomSexpr.cc
	FrameSexpr.cc
	ValueSexpr.cc
)

ADD_DEPENDENCIES(sexpr opencog_atom_types)

TARGET_LINK_LIBRARIES(sexpr
	atomspace
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
