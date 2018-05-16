
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

# Explicitly link every library that defines atom types.
# We want to list every possible library that defines some
# atom type, so that the library ctors will run, and add those
# atom types to the classserver. If this is not done, then the
# classserver will fail to run atom factories, viz, will fail
# to run the ctors for C++-backed atom types, resulting in
# screwball failures.

TARGET_LINK_LIBRARIES(atomspace
	${NO_AS_NEEDED}
	truthvalue_
	atombase
	atomcore
	execution_
	lambda_
	${AS_NEEDED}
	smob_
	query_
	PythonEval_
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
