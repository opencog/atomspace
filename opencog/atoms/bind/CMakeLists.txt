
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (lambda SHARED
	AssignLink.cc
	BetaRedex.cc
	BindLink.cc
	ConcreteLink.cc
	DefineLink.cc
	DeleteLink.cc
	PatternUtils.cc
	SatisfactionLink.cc
	ScopeLink.cc
	VariableList.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(lambda opencog_atom_types)

TARGET_LINK_LIBRARIES(lambda
	atomutils
	${COGUTIL_LIBRARY}
)

IF (CYGWIN)
	INSTALL (TARGETS lambda
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ELSE (CYGWIN)
	INSTALL (TARGETS lambda
		LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (CYGWIN)

INSTALL (FILES
	BindLink.h
	ConcreteLink.h
	PatternUtils.h
	SatisfactionLink.h
	VariableList.h
	DESTINATION "include/${PROJECT_NAME}/atoms/bind"
)
