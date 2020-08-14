# Immediately below is some complicated cmake junk to detect if SQL is
# correctly comfigured.  If it is configured, then DB_IS_CONFIGURED
# is set, and the unit tests are executed. If not, then not.  This
# avoids having the unit tests fail for those users who have not
# configured SQL.  To configure SQL, see the README file, and then
# re-run cmake.

FIND_PROGRAM(PSQL_EXECUTABLE psql)
IF (PSQL_EXECUTABLE)
    SET(PG_HOST $ENV{PGHOST})
    SET(PG_PASSWORD $ENV{PGPASSWORD})
    IF(NOT PG_PASSWORD)
        SET(ENV{PGPASSWORD} "cheese")
    ENDIF(NOT PG_PASSWORD)
    IF (PG_HOST)
        EXECUTE_PROCESS(COMMAND ${PSQL_EXECUTABLE} -c "SELECT * from TypeCodes;" -d opencog_test -U opencog_tester
            OUTPUT_VARIABLE OUT
            ERROR_VARIABLE ERR
            RESULT_VARIABLE RC)
    ELSE (PG_HOST)
        # Arghhhh. This is painfully wrong. Forcing -h locahost makes it
        # use tcpip instead of unix domain sockets; to get tcpip to work,
        # users have to alter pg_hba.conf.
        EXECUTE_PROCESS(COMMAND ${PSQL_EXECUTABLE} -c "SELECT * from TypeCodes;" -d opencog_test -U opencog_tester -h localhost
            OUTPUT_VARIABLE OUT
            ERROR_VARIABLE ERR
            RESULT_VARIABLE RC)
    ENDIF(PG_HOST)
    UNSET(ENV{PG_HOST})
    IF(NOT PG_PASSWORD)
        UNSET(ENV{PGPASSWORD})
    ENDIF(NOT PG_PASSWORD)
    
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

LINK_LIBRARIES(
    atomspace
    persist
    persist-sql
)

IF (DB_IS_CONFIGURED)
    MESSAGE(STATUS "Postgres database is configured for unit tests." )

    ADD_CXXTEST(BasicSaveUTest)
    ADD_CXXTEST(ValueSaveUTest)
    ADD_CXXTEST(PersistUTest)
    ADD_CXXTEST(DeleteUTest)

    # AlphaEquivUTest is stubbed out because alpha-equiv in SQL is
    # broken. See issue https://github.com/opencog/atomspace/issues/2755
    # ADD_CXXTEST(AlphaEquivUTest)

    IF (HAVE_GUILE)
        ADD_CXXTEST(FetchUTest)
        TARGET_LINK_LIBRARIES(FetchUTest execution)
    ENDIF (HAVE_GUILE)

    ADD_CXXTEST(MultiPersistUTest)
    ADD_CXXTEST(MultiUserUTest)
    ADD_CXXTEST(QueryPersistUTest)
    ADD_CXXTEST(LargeFlatUTest)
    ADD_CXXTEST(LargeZipfUTest)

ELSE (DB_IS_CONFIGURED)
    MESSAGE(WARNING "Postgres database not configured for unit tests! See the README!")
ENDIF (DB_IS_CONFIGURED)
