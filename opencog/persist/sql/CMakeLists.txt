
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

INSTALL (TARGETS sql-support EXPORT AtomSpaceTargets
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)


set(atomspace_libs ${atomspace_libs} sql-support)

IF (PGSQL_FOUND OR ODBC_FOUND)
	ADD_SUBDIRECTORY(multi-driver)
ENDIF (PGSQL_FOUND OR ODBC_FOUND)

set(atomspace_libs ${atomspace_libs} PARENT_SCOPE)
