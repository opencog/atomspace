#SET_DIRECTORY_PROPERTIES(PROPERTIES CLEAN_NO_CUSTOM true)

OPENCOG_ADD_ATOM_TYPES(atom_types.script 
  atom_types.h
  atom_types.definitions
  atom_types.inheritance
  core_types.scm
  core_types.pyx)
ADD_CUSTOM_TARGET(opencog_atom_types DEPENDS atom_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atomspace SHARED
	atom_types.h
	Atom.cc
	AtomSpace.cc
	AtomSpaceInit.cc
	AtomTable.cc
	AttentionValue.cc
	AttentionBank.cc
	BackingStore.cc
	ClassServer.cc
	FixedIntegerIndex.cc
	FloatValue.cc
	Handle.cc
	HandleSeqIndex.cc
	# HandleSetIndex.cc
	ImportanceIndex.cc
	# IncomingIndex.cc
	Link.cc
	LinkIndex.cc
	LinkValue.cc
	Node.cc
	NodeIndex.cc
	TLB.cc
	TypeIndex.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomspace opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspace
	truthvalue  
	lambda
	${COGUTIL_LIBRARY}
	${Boost_THREAD_LIBRARY}
)

INSTALL (TARGETS atomspace
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Atom.h
	AtomSpace.h
	AtomTable.h
	${CMAKE_CURRENT_BINARY_DIR}/atom_types.h
	atom_types.cc
	AttentionValue.h
	AttentionBank.h
	BackingStore.h
	ClassServer.h
	FixedIntegerIndex.h
	FloatValue.h
	Handle.h
	HandleSeqIndex.h
	# HandleSetIndex.h
	ImportanceIndex.h
	# IncomingIndex.h
	Link.h
	LinkIndex.h
	LinkValue.h
	NameIndex.h
	Node.h
	NodeIndex.h
	ProtoAtom.h
	StringIndex.h
	TLB.h
	TypeIndex.h
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
