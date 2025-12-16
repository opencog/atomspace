
ADD_GUILE_MODULE (FILES
	opencog.scm
	opencog/base/atom-cache.scm
	opencog/base/core-docs.scm
	opencog/base/repl-shell.scm
	opencog/base/types.scm
	opencog/base/utilities.scm
	DEPENDS opencog_atom_types smob
	COMPILE
)

# Each of the files below are distinct modules. They need to be
# compiled separately.

ADD_GUILE_MODULE (FILES
	opencog/extension.scm
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/test-runner.scm
#	COMPILE
)
