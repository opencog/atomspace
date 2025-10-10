ADD_LIBRARY (truthvalue
	SimpleTruthValue.cc
	TruthValue.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(truthvalue opencog_atom_types)

TARGET_LINK_LIBRARIES(truthvalue
	value
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS truthvalue EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	SimpleTruthValue.h
	TruthValue.h
	DESTINATION "include/opencog/atoms/truthvalue"
)
