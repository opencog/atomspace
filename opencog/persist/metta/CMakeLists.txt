
# Generic MeTTa encoding/decoding.
ADD_LIBRARY(metta
	DecodeMeTTa.cc
)

ADD_DEPENDENCIES(metta opencog_atom_types)

TARGET_LINK_LIBRARIES(metta
	atomspace
	execution
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS metta EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	MeTTa.h
	DESTINATION "include/opencog/persist/metta"
)

# -------------------------------
