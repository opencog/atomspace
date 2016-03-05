# Uncomment this if SQL is installed.
# ADD_DEFINITIONS(-DHAVE_SQL_STORAGE)

INCLUDE_DIRECTORIES (
	${ODBC_INCLUDE_DIRS}
)

ADD_LIBRARY (persist-sql
	odbcxx
	AtomStorage
	SQLBackingStore
	ODBCAtomStorage
	SQLPersistSCM
)

ADD_DEPENDENCIES(persist-sql opencog_atom_types)

TARGET_LINK_LIBRARIES(persist-sql
	atomspace
	${ODBC_LIBRARIES}
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(persist-sql smob)
ENDIF (HAVE_GUILE)


ADD_EXECUTABLE(sniff
	sniff.cc
)

TARGET_LINK_LIBRARIES(sniff
	persist-sql
	atomspace
	execution
	clearbox
	smob
	atomcore
	atomutils
	${ODBC_LIBRARIES}
)


INCLUDE_DIRECTORIES (
	${ODBC_INCLUDE_DIRS}
	${PGSQL_INCLUDE_DIRS}
)

ADD_LIBRARY (persist-pgsql
	odbcxx
	AtomStorage
	SQLBackingStore
	PGAtomStorage
	PGSQLPersistSCM
)

ADD_DEPENDENCIES(persist-pgsql opencog_atom_types)

TARGET_LINK_LIBRARIES(persist-pgsql
	atomspace
	${ODBC_LIBRARIES}
	${PGSQL_LIBRARIES}
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(persist-pgsql smob)
ENDIF (HAVE_GUILE)

ADD_EXECUTABLE(test_pg_storage
	test_pg_storage.cc
)

TARGET_LINK_LIBRARIES(test_pg_storage
	persist-pgsql
	atomspace
	atomspaceutils
	execution
	clearbox
	smob
	atomcore
	atomutils
	${PGSQL_LIBRARIES}
)

INSTALL (TARGETS persist-pgsql
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (TARGETS persist-sql
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AtomStorage.h
	ODBCAtomStorage.h
	PGAtomStorage.h
	SQLBackingStore.h
	odbcxx.h
	SQLPersistSCM.h
	PGSQLPersistSCM.h
	DESTINATION "include/opencog/persist/sql"
)
