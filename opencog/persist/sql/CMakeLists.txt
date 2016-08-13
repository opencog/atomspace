# Uncomment this if SQL is installed.
# ADD_DEFINITIONS(-DHAVE_SQL_STORAGE)

ADD_LIBRARY (sql-support
	AtomStorage
	SQLBackingStore
)

ADD_DEPENDENCIES(sql-support opencog_atom_types)

TARGET_LINK_LIBRARIES(sql-support
	atomspace
	${ODBC_LIBRARIES}
)

INSTALL (FILES
	AtomStorage.h
	SQLBackingStore.h
	DESTINATION "include/opencog/persist/sql"
)

INSTALL (TARGETS sql-support
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

IF (ODBC_FOUND)
	ADD_SUBDIRECTORY(odbc)
ENDIF (ODBC_FOUND)

IF (PGSQL_FOUND)
	ADD_SUBDIRECTORY(postgres)
ENDIF (PGSQL_FOUND)
