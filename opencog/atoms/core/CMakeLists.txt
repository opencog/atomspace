
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomcore SHARED
	ArityLink.cc
	AssignLink.cc
	DefineLink.cc
	DeleteLink.cc
	FreeLink.cc
	FunctionLink.cc
	LambdaLink.cc
	PutLink.cc
	RandomChoice.cc
	ScopeLink.cc
	StateLink.cc
	UniqueLink.cc
	Variables.cc
	VariableList.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomcore opencog_atom_types)

TARGET_LINK_LIBRARIES(atomcore
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomcore
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ArityLink.h
	AssignLink.h
	DefineLink.h
	DeleteLink.h
	FreeLink.h
	FunctionLink.h
	LambdaLink.h
	PutLink.h
	RandomChoice.h
	ScopeLink.h
	StateLink.h
	UniqueLink.h
	Variables.h
	VariableList.h
	DESTINATION "include/opencog/atoms/core"
)
