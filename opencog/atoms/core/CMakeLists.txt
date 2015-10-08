
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (atomcore SHARED
	AssignLink.cc
	DefineLink.cc
	DeleteLink.cc
	FreeLink.cc
	FunctionLink.cc
	LambdaLink.cc
	ForAllLink.cc
	PutLink.cc
	ScopeLink.cc
	StateLink.cc
	UniqueLink.cc
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
	AssignLink.h
	DefineLink.h
	DeleteLink.h
	FreeLink.h
	FunctionLink.h
	LambdaLink.h
	ForAllLink.h
	PutLink.h
	ScopeLink.h
	StateLink.h
	UniqueLink.h
	VariableList.h
	DESTINATION "include/opencog/atoms/core"
)
