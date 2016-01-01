#SET_DIRECTORY_PROPERTIES(PROPERTIES CLEAN_NO_CUSTOM true)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atomspace SHARED
	AtomSpace.cc
	AtomSpaceInit.cc
	AtomTable.cc
	AttentionBank.cc
	BackingStore.cc
	FixedIntegerIndex.cc
	HandleSeqIndex.cc
	# HandleSetIndex.cc
	ImportanceIndex.cc
	# IncomingIndex.cc
	LinkIndex.cc
	NodeIndex.cc
	TLB.cc
	TypeIndex.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomspace opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspace
	atombase
	truthvalue  
	lambda
	${COGUTIL_LIBRARY}
	${Boost_THREAD_LIBRARY}
)

INSTALL (TARGETS atomspace
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AtomSpace.h
	AtomTable.h
	AttentionBank.h
	BackingStore.h
	ClassServer.h
	TLB.h
	types.h
	version.h
	DESTINATION "include/opencog/atomspace"
)

# Install the auto-genned atom types as well
INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/core_types.scm
   DESTINATION "${DATADIR}/scm"
)

# Install the atom_types.script
# required to build the HaskellBindings from Hackage
# XXX FIXME this is just plain-old wrong. There are about FIVE or SIX
# of these atom_types.script files; it is not sufficient to just handle
# one of them.  See bug #549
INSTALL (FILES
	${CMAKE_CURRENT_SOURCE_DIR}/atom_types.script
   DESTINATION "${DATADIR}"
)
