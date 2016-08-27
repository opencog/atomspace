#
# Don't built tests which don't have installed libraries...

IF (ODBC_FOUND)
	ADD_SUBDIRECTORY (odbc)
ENDIF (ODBC_FOUND)

IF (PGSQL_FOUND)
	ADD_SUBDIRECTORY (postgres)
ENDIF (PGSQL_FOUND)
