# Uncomment this if SQL is installed.
# ADD_DEFINITIONS(-DHAVE_SQL_STORAGE)

ADD_LIBRARY (persist-sql
	llapi.cc
	SQLAtomDelete.cc
	SQLAtomLoad.cc
	SQLAtomStore.cc
	SQLAtomStorage.cc
	SQLBulk.cc
	SQLSpaces.cc
	SQLTypeMap.cc
	SQLValues.cc
	SQLUUID.cc
	SQLPersistSCM.cc
)

ADD_DEPENDENCIES(persist-sql opencog_atom_types atomspace)

TARGET_LINK_LIBRARIES(persist-sql
	persist
	storage-types
	tlb
	atomspace
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(persist-sql smob)
	ADD_GUILE_EXTENSION(SCM_CONFIG persist-sql "opencog-ext-path-persist-sql")
ENDIF (HAVE_GUILE)

IF (ODBC_FOUND)
	INCLUDE_DIRECTORIES(${ODBC_INCLUDE_DIRS})
	ADD_LIBRARY (persist-odbc odbcxx.cc)
	TARGET_LINK_LIBRARIES(persist-odbc ${ODBC_LIBRARIES})
	TARGET_LINK_LIBRARIES(persist-sql persist-odbc)

	INSTALL (TARGETS persist-odbc
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (ODBC_FOUND)

IF (PGSQL_FOUND)
	INCLUDE_DIRECTORIES(${PGSQL_INCLUDE_DIRS})
	ADD_LIBRARY (persist-pg ll-pg-cxx.cc)
	TARGET_LINK_LIBRARIES(persist-pg ${PGSQL_LIBRARIES})
	TARGET_LINK_LIBRARIES(persist-sql persist-pg)

	INSTALL (TARGETS persist-pg EXPORT AtomSpaceTargets
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (PGSQL_FOUND)

ADD_EXECUTABLE(sniff
	sniff.cc
)

TARGET_LINK_LIBRARIES(sniff
	persist-sql
)
IF (ODBC_FOUND)
	TARGET_LINK_LIBRARIES(sniff persist-odbc)
ENDIF (ODBC_FOUND)
IF (PGSQL_FOUND)
	TARGET_LINK_LIBRARIES(sniff persist-pg)
ENDIF (PGSQL_FOUND)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(sniff smob)
ENDIF (HAVE_GUILE)

INSTALL (TARGETS persist-sql EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
