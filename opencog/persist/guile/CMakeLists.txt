
ADD_LIBRARY (persist SHARED
	PersistSCM.cc
)

TARGET_LINK_LIBRARIES(persist
	atomspace
	smob
)

INSTALL (TARGETS persist
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	PersistSCM.h
	DESTINATION "include/${PROJECT_NAME}/persist/guile"
)
