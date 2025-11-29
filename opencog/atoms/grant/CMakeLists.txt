# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (grant
	DefineLink.cc
	GrantLink.cc
	StateLink.cc
	TypedAtomLink.cc
	UniqueLink.cc
)

ADD_DEPENDENCIES(grant opencog_atom_types)

TARGET_LINK_LIBRARIES(grant
	free
	atomcore
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS grant EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	DefineLink.h
	GrantLink.h
	StateLink.h
	TypedAtomLink.h
	UniqueLink.h
	DESTINATION "include/opencog/atoms/grant"
)
