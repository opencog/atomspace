# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)

INSTALL (FILES
	opencog.scm
	DESTINATION "${DATADIR}/scm/"
)

# Create a symlink to load-path for site packages
# NOTE: This is the first of the two symlinks required for this to work, as per
# https://www.gnu.org/software/guile/manual/guile.html#Installing-Site-Packages
# TODO: When guile-2.2 is supported by distros update the paths accordingly.
INSTALL (CODE
	"EXECUTE_PROCESS(
		COMMAND ${CMAKE_COMMAND} -E make_directory
		\"/usr/share/guile/site/2.0\"
		COMMAND ${CMAKE_COMMAND} -E create_symlink \"${DATADIR}/scm/opencog.scm\" \"/usr/share/guile/site/2.0/opencog.scm\"
		COMMAND ${CMAKE_COMMAND} -E make_directory \"/usr/local/share/guile/site/2.2\"
		COMMAND ${CMAKE_COMMAND} -E create_symlink \"${DATADIR}/scm/opencog.scm\" \"/usr/local/share/guile/site/2.2/opencog.scm\"
	)"
)

INSTALL (FILES
	opencog/base/apply.scm
	opencog/base/atom-cache.scm
	opencog/base/core-docs.scm
	opencog/base/debug-trace.scm
	opencog/base/file-utils.scm
	opencog/base/repl-shell.scm
	opencog/base/tv.scm
	opencog/base/utilities.scm
	DESTINATION "${DATADIR}/scm/opencog/base"
)

INSTALL (FILES
	opencog/attention-bank.scm
	opencog/exec.scm
	opencog/extension.scm
	opencog/logger.scm
	opencog/randgen.scm
	opencog/persist.scm
	opencog/query.scm
	opencog/rule-engine.scm
	DESTINATION "${DATADIR}/scm/opencog"
)

# NOTE: This is the second of the two symlinks required for this to work.
INSTALL (CODE
	"EXECUTE_PROCESS(
		COMMAND ${CMAKE_COMMAND} -E create_symlink \"${DATADIR}/scm/opencog/\" \"/usr/share/guile/site/2.0/opencog\"
		COMMAND ${CMAKE_COMMAND} -E create_symlink \"${DATADIR}/scm/opencog/\" \"/usr/local/share/guile/site/2.2/opencog\"
	)"
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
