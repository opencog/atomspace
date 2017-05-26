# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)

INSTALL (FILES
	opencog.scm
	DESTINATION "${DATADIR}/scm/"
)

INSTALL (FILES
	opencog/base/apply.scm
	opencog/base/av-tv.scm
	opencog/base/core-docs.scm
	opencog/base/debug-trace.scm
	opencog/base/file-utils.scm
	opencog/base/repl-shell.scm
	opencog/base/utilities.scm
	DESTINATION "${DATADIR}/scm/opencog/base"
)

INSTALL (FILES
	opencog/exec.scm
	opencog/extension.scm
	opencog/logger.scm
	opencog/randgen.scm
	opencog/persist.scm
	opencog/query.scm
	opencog/rule-engine.scm
	DESTINATION "${DATADIR}/scm/opencog"
)

# These guile modules spew "libBlahBlah.so not found" errors,
# if the user failed to actually build the needed component.
# So, do not install them if the component is missing.
IF (HAVE_GEARMAN)
	INSTALL (FILES
		opencog/dist-gearman.scm
		DESTINATION "${DATADIR}/scm/opencog"
	)
ENDIF (HAVE_GEARMAN)

IF (PGSQL_FOUND OR ODBC_FOUND)
	INSTALL (FILES
		opencog/persist-sql.scm
		DESTINATION "${DATADIR}/scm/opencog"
	)
ENDIF (PGSQL_FOUND OR ODBC_FOUND)

# Currently, this is broken, and doesn't work. Don't isntall it.
# IF (PGSQL_FOUND)
# 	INSTALL (FILES
# 		opencog/persist-pgsql.scm
# 		DESTINATION "${DATADIR}/scm/opencog"
# 	)
# ENDIF (PGSQL_FOUND)

IF (HAVE_CYTHON)
	INSTALL (FILES
		opencog/python.scm
		DESTINATION "${DATADIR}/scm/opencog"
	)
ENDIF (HAVE_CYTHON)

IF (HAVE_ZMQ)
	INSTALL (FILES
		opencog/persist-zmq.scm
		DESTINATION "${DATADIR}/scm/opencog"
	)
ENDIF (HAVE_ZMQ)

INSTALL (FILES
	opencog/rule-engine/rule-engine-utils.scm
	DESTINATION "${DATADIR}/scm/opencog/rule-engine"
)
