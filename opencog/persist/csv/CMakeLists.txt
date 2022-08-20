
# Generic JSON decoding.
ADD_LIBRARY (csv
	table_read.cc
)

ADD_DEPENDENCIES(csv opencog_atom_types)

TARGET_LINK_LIBRARIES(csv
	atomspace
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS csv EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	table_read.h
	DESTINATION "include/opencog/persist/csv"
)

# -------------------------------
