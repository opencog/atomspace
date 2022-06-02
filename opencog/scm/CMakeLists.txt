# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)

ADD_GUILE_MODULE (FILES
	opencog.scm
	opencog/base/apply.scm
	opencog/base/atom-cache.scm
	opencog/base/core-docs.scm
	opencog/base/debug-trace.scm
	opencog/base/file-utils.scm
	opencog/base/repl-shell.scm
	opencog/base/tv.scm
	opencog/base/types.scm
	opencog/base/utilities.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}"
	DEPENDS opencog_atom_types
)

ADD_SUBDIRECTORY (opencog)
