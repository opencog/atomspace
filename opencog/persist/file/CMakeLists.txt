
# -------------------------------
# File-specific code.

ADD_LIBRARY (load_scm
	fast_load.cc
)

TARGET_LINK_LIBRARIES(load_scm
	sexcom
	sexpr
)

INSTALL (TARGETS load_scm EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	fast_load.h
	DESTINATION "include/opencog/persist/file"
)

# -------------------------------

ADD_LIBRARY (persist-file
	FileStorage.cc
	PersistFileSCM.cc
)

TARGET_LINK_LIBRARIES(persist-file
	persist
	storage-types
	load_scm
	sexcom
	sexpr
	atomspace
	smob
)

ADD_GUILE_EXTENSION(SCM_CONFIG persist-file "opencog-ext-path-persist-file")

ADD_GUILE_MODULE (FILES
   opencog/persist-file.scm
   DEPENDS persist-file
#  COMPILE
)

INSTALL (TARGETS persist-file EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FileStorage.h
	DESTINATION "include/opencog/persist/file"
)
