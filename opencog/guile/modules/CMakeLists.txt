# Assorted scheme modules

ADD_LIBRARY (logger LoggerSCM.cc)

TARGET_LINK_LIBRARIES(logger
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG logger "opencog-ext-path-logger")

# --------------------------------

ADD_LIBRARY (randgen RandGenSCM.cc)

TARGET_LINK_LIBRARIES(randgen
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG randgen "opencog-ext-path-randgen")

# --------------------------------

ADD_LIBRARY (type-utils TypeUtilsSCM.cc)

TARGET_LINK_LIBRARIES(type-utils
	atomcore
	smob
)
ADD_GUILE_EXTENSION(SCM_CONFIG type-utils "opencog-ext-path-type-utils")

# --------------------------------

ADD_LIBRARY (guile-uuid UuidSCM.cc)

TARGET_LINK_LIBRARIES(guile-uuid
	tlb
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG guile-uuid "opencog-ext-path-uuid")

# --------------------------------

ADD_LIBRARY (exec ExecSCM.cc)

TARGET_LINK_LIBRARIES(exec execution smob)

ADD_GUILE_EXTENSION(SCM_CONFIG exec "opencog-ext-path-exec")

# --------------------------------

INSTALL (TARGETS exec logger randgen type-utils guile-uuid
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
