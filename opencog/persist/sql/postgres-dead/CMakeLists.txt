
INCLUDE_DIRECTORIES (
    ${ODBC_INCLUDE_DIRS}
    ${PGSQL_INCLUDE_DIRS}
)

ADD_LIBRARY (persist-pgsql
    odbcxx
    OutgoingHash
    PGAtomStorage
    PGSQLPersistSCM
)

ADD_DEPENDENCIES(persist-pgsql opencog_atom_types)

TARGET_LINK_LIBRARIES(persist-pgsql
    sql-support
    atomspace
    atomspaceutils
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
    sql-support
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

INSTALL (FILES
    PGAtomStorage.h
    PGSQLPersistSCM.h
    DESTINATION "include/opencog/persist/sql/postgres"
)
