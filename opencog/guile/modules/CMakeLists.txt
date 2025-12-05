# Assorted scheme modules
# --------------------------------

# The exec module is now a pure-scheme stub; cog-execute! is provided
# by the core (opencog) module. This stub exists for backward compat.
ADD_GUILE_MODULE (FILES
   opencog/exec.scm
#  COMPILE
)

# --------------------------------

ADD_LIBRARY (logger LoggerSCM.cc)

TARGET_LINK_LIBRARIES(logger
	${GUILE_LIBRARIES}
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG logger "opencog-ext-path-logger")

ADD_GUILE_MODULE (FILES
   opencog/logger.scm
   DEPENDS logger
#  COMPILE
)

# --------------------------------

ADD_LIBRARY (randgen RandGenSCM.cc)

TARGET_LINK_LIBRARIES(randgen
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG randgen "opencog-ext-path-randgen")

ADD_GUILE_MODULE (FILES
   opencog/randgen.scm
   DEPENDS randgen
#  COMPILE
)

# --------------------------------

ADD_LIBRARY (type-utils TypeUtilsSCM.cc)

TARGET_LINK_LIBRARIES(type-utils
	atomcore
	smob
)
ADD_GUILE_EXTENSION(SCM_CONFIG type-utils "opencog-ext-path-type-utils")

ADD_GUILE_MODULE (FILES
   opencog/type-utils.scm
   DEPENDS type-utils
#  COMPILE
)

# --------------------------------

INSTALL (TARGETS logger randgen type-utils
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
