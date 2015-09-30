INCLUDE_DIRECTORIES (
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/persist/file
	${PROJECT_SOURCE_DIR}/opencog/persist/sql
	${PROJECT_SOURCE_DIR}/opencog/util
)

LINK_DIRECTORIES(
	${PROJECT_BINARY_DIR}/opencog/atomspace
	${PROJECT_BINARY_DIR}/opencog/persist
	${PROJECT_BINARY_DIR}/opencog/util
)

LINK_LIBRARIES(
	atomspace
	persist-sql
)

# The two tests below currrently work and pass, as of 10 December 2013.
# There's this complicated cmake junk, because they will fail for
# anyone who hasn't corrrectly configured their SQL setup.  To run
# these tests, see the README file, and re-run cmake.

FIND_PROGRAM(PSQL_EXECUTABLE psql)
IF (PSQL_EXECUTABLE)
	SET(PG_HOST $ENV{PGHOST})
	IF (PG_HOST)
		EXECUTE_PROCESS(COMMAND ${PSQL_EXECUTABLE} -c "SELECT * from TypeCodes;" -d opencog_test -U opencog_tester
			OUTPUT_VARIABLE OUT
			ERROR_VARIABLE ERR
			RESULT_VARIABLE RC)
	ELSE (PG_HOST)
		EXECUTE_PROCESS(COMMAND ${PSQL_EXECUTABLE} -c "SELECT * from TypeCodes;" -d opencog_test -U opencog_tester -h localhost
			OUTPUT_VARIABLE OUT
			ERROR_VARIABLE ERR
			RESULT_VARIABLE RC)
	ENDIF(PG_HOST)
	UNSET(ENV{PG_HOST})

	# Check the return code from PSQL; if it failed, try a second time.
	# The most likely cause of failure is that "-h localhost" requires
	# a tcp socket to be open; it might not be. So we retest; then,
	# without the PGHOST and without the "-h localhost", the unix-domain
	# socket will get used.
	IF (RC)
		EXECUTE_PROCESS(COMMAND ${PSQL_EXECUTABLE} -c "SELECT * from TypeCodes;" -d opencog_test -U opencog_tester
			OUTPUT_VARIABLE OUT
			ERROR_VARIABLE ERR
			RESULT_VARIABLE RC)
	ENDIF (RC)

	# Check the return code from PSQL
	IF (RC)
		MESSAGE(WARNING "${PSQL_EXECUTABLE}: ${ERR}")
	ELSE (RC)
		SET(DB_IS_CONFIGURED 1)
	ENDIF (RC)

ENDIF (PSQL_EXECUTABLE)

# If above doesn't work, then uncomment this line.
# SET(DB_IS_CONFIGURED 1)

IF (DB_IS_CONFIGURED)
	ADD_CXXTEST(BasicSaveUTest)
	ADD_CXXTEST(PersistUTest)
	MESSAGE(STATUS "Postgres database is configured for unit tests." )
ELSE (DB_IS_CONFIGURED)
	MESSAGE(WARNING "Postgres database not configured for unit tests! See the README!")
ENDIF (DB_IS_CONFIGURED)
