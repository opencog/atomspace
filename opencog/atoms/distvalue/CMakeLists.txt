ADD_LIBRARY (distvalue
	DistributionalValue.cc
	ConditionalDV.cc
	DVFormulas.cc
    DistributionalValueSCM.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(distvalue opencog_atom_types)

TARGET_LINK_LIBRARIES(distvalue
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS distvalue EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	DistributionalValue.h
	ConditionalDV.h
	DVFormulas.h
    DistributionalValueSCM.h
	DESTINATION "include/opencog/atoms/distvalue"
)
