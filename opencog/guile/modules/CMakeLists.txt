
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
	ruleengine # for cog-ure-logger XXX FIXME this is insane. WTF!??
	smob
	${COGUTIL_LIBRARY}
)

TARGET_LINK_LIBRARIES(randgen
	smob
	${COGUTIL_LIBRARY}
)

TARGET_LINK_LIBRARIES(type-utils
	atomcore
	smob
)

INSTALL (TARGETS logger randgen type-utils
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
