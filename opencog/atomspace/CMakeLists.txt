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
	CountTruthValue.cc
	FixedIntegerIndex.cc
	FuzzyTruthValue.cc
	Handle.cc
	HandleSeqIndex.cc
	HandleSetIndex.cc
	ImportanceIndex.cc
	IncomingIndex.cc
	IndefiniteTruthValue.cc
	Link.cc
	LinkIndex.cc
	Node.cc
	NodeIndex.cc
	NullTruthValue.cc
	ProbabilisticTruthValue.cc
	SimpleTruthValue.cc
	TLB.cc
	TruthValue.cc
	TypeIndex.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomspace opencog_atom_types atom_types.h)

TARGET_LINK_LIBRARIES(atomspace
	lambda
	${COGUTIL_LIBRARY}
	${GSL_LIBRARIES}
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
	CountTruthValue.h
	FixedIntegerIndex.h
	FuzzyTruthValue.h
	Handle.h
	HandleSeqIndex.h
	HandleSetIndex.h
	ImportanceIndex.h
	IncomingIndex.h
	IndefiniteTruthValue.h
	Link.h
	LinkIndex.h
	NameIndex.h
	Node.h
	NodeIndex.h
	NullTruthValue.h
	ProbabilisticTruthValue.h
	SimpleTruthValue.h
	StringIndex.h
	TLB.h
	TruthValue.h
	TypeIndex.h
	types.h
	version.h
	DESTINATION "include/${PROJECT_NAME}/atomspace"
)

# Install the auto-genned atom types as well
INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/core_types.scm
   DESTINATION "${DATADIR}/scm"
)
