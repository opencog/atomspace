#
# If SQL is not installed, then don't build the tests.

IF(HAVE_SQL_STORAGE)

	# Create a test configuration file by copying the default config file
	CONFIGURE_FILE(atomspace-test.conf
	               ${PROJECT_BINARY_DIR}/lib/atomspace-test.conf)

	ADD_SUBDIRECTORY (multi-driver)
ENDIF(HAVE_SQL_STORAGE)
