
ADD_LIBRARY (persist
	PersistSCM.cc
)

TARGET_LINK_LIBRARIES(persist
	atomspace
	smob
)

INSTALL (TARGETS persist
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	PersistSCM.h
	DESTINATION "include/opencog/persist/guile"
)
