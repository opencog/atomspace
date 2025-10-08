
ADD_LIBRARY (backing SHARED
	BackingQuery.cc
	BackingStore.cc
	StorageNode.cc
)

TARGET_LINK_LIBRARIES(backing
	atomspace
	smob
)

INSTALL (TARGETS persist EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	BackingStore.h
	StorageNode.h
   DESTINATION "include/opencog/persist/api"
)
