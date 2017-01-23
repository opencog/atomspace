#
# Don't built tests which don't have installed libraries...


IF(HAVE_SQL_STORAGE)
	ADD_SUBDIRECTORY (multi-driver)
ENDIF(HAVE_SQL_STORAGE)

# This is just horridly borkn...
#IF (PGSQL_FOUND)
#	ADD_SUBDIRECTORY (postgres)
#ENDIF (PGSQL_FOUND)
