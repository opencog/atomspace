
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (scope
	Context.cc
	FilterVardecl.cc
	LambdaLink.cc
	PrenexLink.cc
	PutLink.cc
	RewriteLink.cc
	ScopeLink.cc
	TypedVariableLink.cc
	Variables.cc
	VariableList.cc
	VariableSet.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(scope opencog_atom_types)

TARGET_LINK_LIBRARIES(scope
	free
	signature
	grant
	atomcore
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS scope EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	Context.h
	FilterVardecl.h
	LambdaLink.h
	PrenexLink.h
	PutLink.h
	RewriteLink.h
	ScopeLink.h
	TypedVariableLink.h
	Variables.h
	VariableList.h
	VariableSet.h
	DESTINATION "include/opencog/atoms/scope"
)
