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
	DESTINATION "${DATADIR}/scm"
)
INSTALL (FILES
	opencog/extension.scm
	opencog/persist.scm
	opencog/persist-sql.scm
	opencog/query.scm
	opencog/rule-engine.scm
	DESTINATION "${DATADIR}/scm/opencog"
)
# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)
