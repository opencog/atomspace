# No CMakeLists.txt in dir:
#ADD_SUBDIRECTORY (wires)

# About the COMPILE flag in the macros below:
# It causes the guild compiler to run and build a compiled version of
# the module. It works. But guile does not actually use this compiled
# version, because, after install, it finds that the sources have a
# newer timestamp.  I don't feel like wrestling with this right now,
# so ... for just right now, disable this. It should be fixed and
# re-enabled.

ADD_GUILE_MODULE (FILES
	opencog.scm
	opencog/base/apply.scm
	opencog/base/atom-cache.scm
	opencog/base/atom-docs.scm
	opencog/base/core-docs.scm
	opencog/base/debug-trace.scm
	opencog/base/file-utils.scm
	opencog/base/repl-shell.scm
	opencog/base/tv.scm
	opencog/base/types.scm
	opencog/base/utilities.scm
	DEPENDS opencog_atom_types smob
#	COMPILE
)

# Each of the files below are distinct modules. They need to be
# compiled seperately.

ADD_GUILE_MODULE (FILES
	opencog/extension.scm
#	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/query.scm
#	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/test-runner.scm
#	COMPILE
)
