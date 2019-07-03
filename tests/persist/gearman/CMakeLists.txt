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
	dist-gearman
)

# Make sure that the Gearman job server is running.
# See the README file for details.
FIND_PROGRAM(GEARMAN_JOB_SERVER gearmand)
IF (GEARMAN_JOB_SERVER)
	ADD_CONFIGURABLE_CXXTEST(GearmanUTest)

	ADD_DEPENDENCIES(tests GearmanUTest)

	ADD_TEST(NAME GearmanUTest
		COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/run_gearman_test.sh
			${CMAKE_CURRENT_BINARY_DIR}
	)
	SET_TESTS_PROPERTIES(GearmanUTest
		PROPERTIES
		ENVIRONMENT "GUILE_LOAD_PATH=${GUILE_LOAD_PATH}"
	)

ENDIF (GEARMAN_JOB_SERVER)
