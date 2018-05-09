
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atomspace
	AtomSpace.cc
	AtomSpaceInit.cc
	AtomTable.cc
	BackingStore.cc
	FixedIntegerIndex.cc
	TypeIndex.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomspace opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspace
#	${NO_AS_NEEDED}
	atomcore
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
	TypeIndex.h
	version.h
	DESTINATION "include/opencog/atomspace"
)
