
# Each of the files below are distinct modules. They need to be
# compiled seperately.
ADD_GUILE_MODULE (FILES
	exec.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	extension.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	logger.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	randgen.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	query.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	test-runner.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	type-utils.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	uuid.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

# Need to recompile persist.scm whenever storage_types.scm changes
# However, no matter what I do, I can't get the rebuild to trigger.
# In other words, the persist module is badly broken.
ADD_GUILE_MODULE (FILES
	persist.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	DEPENDS storage_types
)

ADD_GUILE_MODULE (FILES
	persist-file.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

# These guile modules spew "libBlahBlah.so not found" errors,
# if the user failed to actually build the needed component.
# So, do not install them if the component is missing.
IF (HAVE_GEARMAN)
	ADD_GUILE_MODULE (FILES
		dist-gearman.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (HAVE_GEARMAN)

IF (HAVE_SQL_STORAGE)
	ADD_GUILE_MODULE (FILES
		persist-sql.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (HAVE_SQL_STORAGE)

IF (HAVE_CYTHON)
	ADD_GUILE_MODULE (FILES
		python.scm
		MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
	)
ENDIF (HAVE_CYTHON)
