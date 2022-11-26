
ADD_LIBRARY (persist
	BackingQuery.cc
	BackingStore.cc
	PersistSCM.cc
	StorageNode.cc
)

TARGET_LINK_LIBRARIES(persist
	storage-types
	atomspace
	smob

	# The ${NO_AS_NEEDED} causes this library to be force-linked
	# with all symbols in it. We do this for two reasons: to include
	# the definitions for FetchValueOf, etc to be part of the persist
	# module, and also so that the shared-lib ctors run for the
	# persistflow lib, as otherwise, the Atom factory is not set up.
	${NO_AS_NEEDED}
	persistflow
)

ADD_GUILE_EXTENSION(SCM_CONFIG persist "opencog-ext-path-persist")

# Need to recompile persist.scm whenever storage_types.scm changes
# However, no matter what I do, I can't get the rebuild to trigger.
# In other words, the persist module is badly broken.
ADD_GUILE_MODULE (FILES
	opencog/persist.scm
	DEPENDS storage_types persist
#	COMPILE
)

INSTALL (TARGETS persist EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	BackingStore.h
	StorageNode.h
	PersistSCM.h
   DESTINATION "include/opencog/persist/api"
)
