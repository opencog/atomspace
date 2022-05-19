
# Generic JSON decoding.
ADD_LIBRARY (json
	DecodeJson.cc
	EncodeJson.cc
	JSCommands.cc
	JsonEval.cc
)

ADD_DEPENDENCIES(json opencog_atom_types)

TARGET_LINK_LIBRARIES(json
	atomspace
	execution
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS json EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	JSCommands.h
	JsonEval.h
	Json.h
	DESTINATION "include/opencog/persist/json"
)

# -------------------------------
