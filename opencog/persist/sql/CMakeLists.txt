
ADD_LIBRARY (sql-support
	AtomStorage
	SQLBackingStore
)

ADD_DEPENDENCIES(sql-support opencog_atom_types)

TARGET_LINK_LIBRARIES(sql-support
	atomspace
)

INSTALL (FILES
	AtomStorage.h
	SQLBackingStore.h
	DESTINATION "include/opencog/persist/sql"
)

INSTALL (TARGETS sql-support
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

IF (PGSQL_FOUND OR ODBC_FOUND)
	ADD_SUBDIRECTORY(multi-driver)
ENDIF (PGSQL_FOUND OR ODBC_FOUND)
