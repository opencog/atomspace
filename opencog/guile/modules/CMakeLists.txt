
ADD_LIBRARY (logger
	LoggerSCM.cc
)

ADD_LIBRARY (randgen
	RandGenSCM.cc
)

ADD_LIBRARY (type-utils
	TypeUtilsSCM.cc
)

TARGET_LINK_LIBRARIES(logger
	ure # for cog-ure-logger XXX FIXME this is insane. WTF!??
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

INSTALL (TARGETS logger randgen type-utils
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
