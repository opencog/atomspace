
# Generic Datalog/prolog decoding.
ADD_LIBRARY(datalog
	DecodeProlog.cc
)

ADD_DEPENDENCIES(datalog opencog_atom_types)

TARGET_LINK_LIBRARIES(datalog
	atomspace
	execution
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS datalog EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Prolog.h
	DESTINATION "include/opencog/persist/prolog"
)

# -------------------------------
