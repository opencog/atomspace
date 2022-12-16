
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomcore
	AbsentLink.cc
	Checkers.cc
	CondLink.cc
	Context.cc
	DefineLink.cc
	DeleteLink.cc
	FindUtils.cc
	FreeLink.cc
	FreeVariables.cc
	FunctionLink.cc
	LambdaLink.cc
	NumberNode.cc
	PrenexLink.cc
	PresentLink.cc
	PutLink.cc
	Quotation.cc
	RandomChoice.cc
	Replacement.cc
	RewriteLink.cc
	ScopeLink.cc
	SleepLink.cc
	StateLink.cc
	TimeLink.cc
	TypeChoice.cc
	TypedAtomLink.cc
	TypedVariableLink.cc
	TypeIntersectionLink.cc
	TypeNode.cc
	TypeUtils.cc
	UniqueLink.cc
	UnorderedLink.cc
	Variables.cc
	VariableList.cc
	VariableSet.cc
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
	Context.h
	DefineLink.h
	DeleteLink.h
	FindUtils.h
	FreeLink.h
	FreeVariables.h
	FunctionLink.h
	LambdaLink.h
	NumberNode.h
	PrenexLink.h
	PresentLink.h
	PutLink.h
	Quotation.h
	RandomChoice.h
	Replacement.h
	RewriteLink.h
	ScopeLink.h
	SleepLink.h
	StateLink.h
	TimeLink.h
	TypeChoice.h
	TypedAtomLink.h
	TypedVariableLink.h
	TypeIntersectionLink.h
	TypeNode.h
	TypeUtils.h
	UniqueLink.h
	UnorderedLink.h
	Variables.h
	VariableList.h
	VariableSet.h
	DESTINATION "include/opencog/atoms/core"
)
