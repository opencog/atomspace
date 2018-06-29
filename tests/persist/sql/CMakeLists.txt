#
# If SQL is not installed, then don't build the tests.

IF(HAVE_SQL_STORAGE)
	ADD_SUBDIRECTORY (multi-driver)
ENDIF(HAVE_SQL_STORAGE)
