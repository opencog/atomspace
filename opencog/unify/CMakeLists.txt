ADD_LIBRARY (unify
	Unify
)

ADD_DEPENDENCIES(unify opencog_atom_types)

TARGET_LINK_LIBRARIES(unify
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (FILES
	Unify.h
	DESTINATION "include/opencog/unify"
)
