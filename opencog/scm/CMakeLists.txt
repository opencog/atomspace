# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)

ADD_GUILE_MODULE (FILES
	opencog.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/"
)

ADD_GUILE_MODULE (FILES
	opencog/base/apply.scm
	opencog/base/atom-cache.scm
	opencog/base/core-docs.scm
	opencog/base/debug-trace.scm
	opencog/base/file-utils.scm
	opencog/base/repl-shell.scm
	opencog/base/tv.scm
	opencog/base/utilities.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/base"
)

ADD_GUILE_MODULE (FILES
	opencog/exec.scm
	opencog/extension.scm
	opencog/logger.scm
	opencog/randgen.scm
	opencog/persist.scm
	opencog/query.scm
	opencog/rule-engine.scm
	opencog/test-runner.scm
	opencog/type-utils.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

# These guile modules spew "libBlahBlah.so not found" errors,
# if the user failed to actually build the needed component.
# So, do not install them if the component is missing.
IF (HAVE_GEARMAN)
	ADD_GUILE_MODULE (FILES
		opencog/dist-gearman.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (HAVE_GEARMAN)

IF (PGSQL_FOUND OR ODBC_FOUND)
	ADD_GUILE_MODULE (FILES
		opencog/persist-sql.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (PGSQL_FOUND OR ODBC_FOUND)

IF (HAVE_CYTHON)
	ADD_GUILE_MODULE (FILES
		opencog/python.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (HAVE_CYTHON)

IF (HAVE_ZMQ)
	ADD_GUILE_MODULE (FILES
		opencog/persist-zmq.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (HAVE_ZMQ)

ADD_GUILE_MODULE (FILES
	opencog/rule-engine/rule-engine-utils.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/rule-engine"
)
