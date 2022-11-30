
# S-expression command dispatcher
ADD_LIBRARY (sexcom
	Commands.cc
	Dispatcher.cc
)

ADD_DEPENDENCIES(sexcom opencog_atom_types)
ADD_DEPENDENCIES(sexcom storage_types)

TARGET_LINK_LIBRARIES(sexcom
	persist
	atomspace
	execution
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS sexcom EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Commands.h
	Dispatcher.h
	DESTINATION "include/opencog/persist/sexcom"
)

# -------------------------------
