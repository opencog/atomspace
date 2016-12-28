
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atomspace
	AtomSpace.cc
	AtomSpaceInit.cc
	AtomTable.cc
	BackingStore.cc
	FixedIntegerIndex.cc
	ThreadSafeFixedIntegerIndex.cc
	TypeIndex.cc

	# The below are no longer used, but we will
	# keep them around for a little while longer... just in case!?
	# HandleSeqIndex.cc
	# HandleSetIndex.cc
	# IncomingIndex.cc
	# LinkIndex.cc
	# NodeIndex.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomspace opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspace
	-Wl,--no-as-needed
	atomcore
	lambda
	clearbox
	atomutils
	atombase
	truthvalue
	${COGUTIL_LIBRARY}
	${Boost_THREAD_LIBRARY}
)

INSTALL (TARGETS atomspace
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AtomSpace.h
	AtomTable.h
	BackingStore.h
	FixedIntegerIndex.h
	ThreadSafeFixedIntegerIndex.h
	TypeIndex.h
	version.h
	DESTINATION "include/opencog/atomspace"
)
