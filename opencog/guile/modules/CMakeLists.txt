
ADD_LIBRARY (logger
	LoggerSCM.cc
)

ADD_LIBRARY (randgen
	RandGenSCM.cc
)

ADD_LIBRARY (type-utils
	TypeUtilsSCM.cc
)

ADD_LIBRARY (guile-uuid
	UuidSCM.cc
)

TARGET_LINK_LIBRARIES(logger
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG logger "opencog-ext-path-logger")

TARGET_LINK_LIBRARIES(randgen
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG randgen "opencog-ext-path-randgen")

TARGET_LINK_LIBRARIES(type-utils
	atomcore
	smob
)
ADD_GUILE_EXTENSION(SCM_CONFIG type-utils "opencog-ext-path-type-utils")

TARGET_LINK_LIBRARIES(guile-uuid
	atomspaceutils
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG guile-uuid "opencog-ext-path-uuid")

INSTALL (TARGETS logger randgen type-utils guile-uuid
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
