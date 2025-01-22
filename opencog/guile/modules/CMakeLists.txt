# Assorted scheme modules
# --------------------------------

ADD_LIBRARY (exec ExecSCM.cc)

TARGET_LINK_LIBRARIES(exec execution smob)

ADD_GUILE_EXTENSION(SCM_CONFIG exec "opencog-ext-path-exec")

ADD_GUILE_MODULE (FILES
   opencog/exec.scm
   DEPENDS exec
#  COMPILE
)

# --------------------------------

ADD_LIBRARY (logger LoggerSCM.cc)

TARGET_LINK_LIBRARIES(logger
	smob
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

ADD_LIBRARY (guile-uuid UuidSCM.cc)

TARGET_LINK_LIBRARIES(guile-uuid
	#tlb
	smob
	${COGUTIL_LIBRARY}
)
ADD_GUILE_EXTENSION(SCM_CONFIG guile-uuid "opencog-ext-path-uuid")

ADD_GUILE_MODULE (FILES
   opencog/uuid.scm
   DEPENDS guile-uuid
#  COMPILE
)

# --------------------------------

INSTALL (TARGETS exec logger randgen type-utils guile-uuid
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
