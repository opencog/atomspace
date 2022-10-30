
# Generic S-expression decoding.
ADD_LIBRARY (sexpr
	AtomSexpr.cc
	Commands.cc
	Dispatcher.cc
	FrameSexpr.cc
	SexprEval.cc
	ValueSexpr.cc
)

ADD_DEPENDENCIES(sexpr opencog_atom_types)

TARGET_LINK_LIBRARIES(sexpr
	atomspace
	execution
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS sexpr EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Commands.h
	Dispatcher.h
	Sexpr.h
	SexprEval.h
	DESTINATION "include/opencog/persist/sexpr"
)

# -------------------------------
