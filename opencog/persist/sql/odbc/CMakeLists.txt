# Uncomment this if SQL is installed.
# ADD_DEFINITIONS(-DHAVE_SQL_STORAGE)

ADD_LIBRARY (persist-sql
	llapi
	SQLAtomStorage
	SQLPersistSCM
)

ADD_DEPENDENCIES(persist-sql opencog_atom_types)

TARGET_LINK_LIBRARIES(persist-sql
	sql-support
	atomspaceutils
	atomspace
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(persist-sql smob)
ENDIF (HAVE_GUILE)

IF (ODBC_FOUND)
	INCLUDE_DIRECTORIES ( ${ODBC_INCLUDE_DIRS} )
	ADD_LIBRARY (persist-odbc
		odbcxx
	)
	TARGET_LINK_LIBRARIES(persist-odbc ${ODBC_LIBRARIES})
	TARGET_LINK_LIBRARIES(persist-sql persist-odbc)
ENDIF (ODBC_FOUND)


ADD_EXECUTABLE(sniff
	sniff.cc
)

TARGET_LINK_LIBRARIES(sniff
	sql-support
	persist-sql
	atomspace
	execution
	clearbox
	atomcore
	atomutils
)
IF (ODBC_FOUND)
	TARGET_LINK_LIBRARIES(sniff ${ODBC_LIBRARIES})
ENDIF (ODBC_FOUND)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(sniff smob)
ENDIF (HAVE_GUILE)

INSTALL (TARGETS persist-sql
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	llapi.h
	SQLAtomStorage.h
	SQLPersistSCM.h
	DESTINATION "include/opencog/persist/sql/odbc"
)
