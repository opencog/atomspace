INCLUDE_DIRECTORIES (
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/persist/gearman
)

LINK_DIRECTORIES (
	${PROJECT_BINARY_DIR}/opencog/atomspace
)

LINK_LIBRARIES (
	smob
	atomspace
)

# Make sure that the Gearman job server is running.
# See the README file for details.
FIND_PROGRAM(GEARMAN_JOB_SERVER gearmand)
IF (GEARMAN_JOB_SERVER)
	EXECUTE_PROCESS(COMMAND "ps" "-C" "gearmand"
		OUTPUT_VARIABLE OUT
		ERROR_VARIABLE ERR
		RESULT_VARIABLE RC)

	IF (RC)
		MESSAGE(WARNING "gearman-job-server is not running; will not run the GearmanUTest")
	ELSE (RC)
		SET(GEARMAN_RUNNING 1)
		MESSAGE(STATUS "gearman-job-server found")
	ENDIF (RC)

ENDIF (GEARMAN_JOB_SERVER)

IF (XGEARMAN_RUNNING)
	ADD_CXXTEST(GearmanUTest)
ENDIF (XGEARMAN_RUNNING)
