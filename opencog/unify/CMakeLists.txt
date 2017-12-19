ADD_LIBRARY (unify
	Unify
)

ADD_DEPENDENCIES(unify opencog_atom_types)

TARGET_LINK_LIBRARIES(unify
	lambda
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS unify DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Unify.h
	DESTINATION "include/opencog/unify"
)
