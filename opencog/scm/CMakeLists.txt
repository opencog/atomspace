# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)

INSTALL (FILES
	apply.scm
	av-tv.scm
	config.scm
	core-docs.scm
	example-dbi.scm
	file-utils.scm
	opencog.scm
	persistence.scm
	repl-shell.scm
	utilities.scm
	rule-engine-utils.scm
	logger-utils.scm
	DESTINATION "${DATADIR}/scm"
)

INSTALL (FILES
	opencog/dist-gearman.scm
	opencog/exec.scm
	opencog/extension.scm
	opencog/persist.scm
	opencog/persist-sql.scm
	opencog/persist-zmq.scm
	opencog/query.scm
	opencog/rule-engine.scm
	opencog/logger.scm
	DESTINATION "${DATADIR}/scm/opencog"
)
