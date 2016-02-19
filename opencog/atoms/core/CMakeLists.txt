
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomcore
	ArityLink.cc
	DefineLink.cc
	DeleteLink.cc
	FreeLink.cc
	FunctionLink.cc
	ImplicationLink.cc
	LambdaLink.cc
	MapLink.cc
	PutLink.cc
	RandomChoice.cc
	RandomNumber.cc
	ScopeLink.cc
	SleepLink.cc
	StateLink.cc
	TimeLink.cc
	TypedAtomLink.cc
	UniqueLink.cc
	Variables.cc
	VariableList.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomcore opencog_atom_types)

TARGET_LINK_LIBRARIES(atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomcore
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ArityLink.h
	DefineLink.h
	DeleteLink.h
	FreeLink.h
	FunctionLink.h
	LambdaLink.h
	MapLink.h
	PutLink.h
	RandomChoice.h
	RandomNumber.h
	ScopeLink.h
	SleepLink.h
	StateLink.h
	TimeLink.h
	TypedAtomLink.h
	UniqueLink.h
	Variables.h
	VariableList.h
	DESTINATION "include/opencog/atoms/core"
)
