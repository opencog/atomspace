
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# Optional replacement for std::set
IF (HAVE_FOLLY)
	INCLUDE_DIRECTORIES(${FOLLY_INCLUDE_DIR})
ENDIF (HAVE_FOLLY)

ADD_LIBRARY (atomspace
	AtomSpace.cc
	AtomTable.cc
	Frame.cc
	Transient.cc
	TypeIndex.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomspace opencog_atom_types)

TARGET_LINK_LIBRARIES(atomspace
	# We want to list every possible library that defines some
	# atom type, so that the library ctors will run, and add those
	# atom types to the classserver. If this is not done, then the
	# classserver will fail to run atom factories, viz, will fail
	# to run the ctors for C++-backed atom types, resulting in
	# screwball failures.
	${NO_AS_NEEDED}
	clearbox
	join
	pattern
	grounded
	execution
	atomflow
	atomcore
	atombase
	truthvalue
	${COGUTIL_LIBRARY}
)

IF (HAVE_FOLLY)
	TARGET_LINK_LIBRARIES(atomspace ${FOLLY_LIBRARIES})
ENDIF (HAVE_FOLLY)

INSTALL (TARGETS atomspace EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AtomSpace.h
	Frame.h
	Transient.h
	TypeIndex.h
	version.h
	DESTINATION "include/opencog/atomspace"
)
