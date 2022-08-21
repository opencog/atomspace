
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

ADD_LIBRARY (csv-table
   TableSCM.cc
)

TARGET_LINK_LIBRARIES(csv-table
	csv
	atomspace
	smob
)

ADD_GUILE_EXTENSION(SCM_CONFIG csv-table "opencog-ext-path-csv-table")

INSTALL (TARGETS csv-table EXPORT AtomSpaceTargets
   DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

# -------------------------------
