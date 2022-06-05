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
	DEPENDS opencog_atom_types smob
	COMPILE
)

# Each of the files below are distinct modules. They need to be
# compiled seperately.
ADD_GUILE_MODULE (FILES
	opencog/exec.scm
	DEPENDS opencog_go exec
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/extension.scm
	DEPENDS opencog_go
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/logger.scm
	DEPENDS opencog_go logger
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/randgen.scm
	DEPENDS opencog_go randgen
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/query.scm
	DEPENDS opencog_go
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/test-runner.scm
	DEPENDS opencog_go
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/type-utils.scm
	DEPENDS opencog_go type-utils
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/uuid.scm
	DEPENDS opencog_go guile-uuid
	COMPILE
)

# Need to recompile persist.scm whenever storage_types.scm changes
# However, no matter what I do, I can't get the rebuild to trigger.
# In other words, the persist module is badly broken.
ADD_GUILE_MODULE (FILES
	opencog/persist.scm
	DEPENDS storage_types
	COMPILE
)

ADD_GUILE_MODULE (FILES
	opencog/persist-file.scm
	COMPILE
)

# These guile modules spew "libBlahBlah.so not found" errors,
# if the user failed to actually build the needed component.
# So, do not install them if the component is missing.
IF (HAVE_GEARMAN)
	ADD_GUILE_MODULE (FILES
		opencog/dist-gearman.scm
		COMPILE
	)
ENDIF (HAVE_GEARMAN)

IF (HAVE_SQL_STORAGE)
	ADD_GUILE_MODULE (FILES
		opencog/persist-sql.scm
		COMPILE
	)
ENDIF (HAVE_SQL_STORAGE)

IF (HAVE_CYTHON)
	ADD_GUILE_MODULE (FILES
		opencog/python.scm
		COMPILE
	)
ENDIF (HAVE_CYTHON)
