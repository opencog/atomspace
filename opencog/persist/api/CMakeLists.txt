
ADD_LIBRARY (backing SHARED
	BackingQuery.cc
	BackingStore.cc
	StorageNode.cc
)

TARGET_LINK_LIBRARIES(backing
	atomspace
	smob
)

INSTALL (TARGETS backing EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

# Historically, this wa named "persist", and, to avoid compat
# issues, install into the historical directory.
INSTALL (FILES
	BackingStore.h
	StorageNode.h
   DESTINATION "include/opencog/persist/api"
)
