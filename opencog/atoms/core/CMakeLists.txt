
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomcore
	AbsentLink.cc
	Checkers.cc
	CondLink.cc
	DefineLink.cc
	DeleteLink.cc
	FindUtils.cc
	FreeLink.cc
	FreeVariables.cc
	FunctionLink.cc
	GrantLink.cc
	NumberNode.cc
	PresentLink.cc
	RandomChoice.cc
	StateLink.cc
	TimeLink.cc
	TypeChoice.cc
	TypedAtomLink.cc
	TypeIntersectionLink.cc
	TypeNode.cc
	TypeUtils.cc
	UniqueLink.cc
	UnorderedLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomcore opencog_atom_types)

TARGET_LINK_LIBRARIES(atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomcore EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AbsentLink.h
	CondLink.h
	DefineLink.h
	DeleteLink.h
	FindUtils.h
	FreeLink.h
	FreeVariables.h
	FunctionLink.h
	GrantLink.h
	NumberNode.h
	PresentLink.h
	RandomChoice.h
	StateLink.h
	TimeLink.h
	TypeChoice.h
	TypedAtomLink.h
	TypeIntersectionLink.h
	TypeNode.h
	TypeUtils.h
	UniqueLink.h
	UnorderedLink.h
	DESTINATION "include/opencog/atoms/core"
)
