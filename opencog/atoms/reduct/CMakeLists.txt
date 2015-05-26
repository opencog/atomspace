
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (clearbox STATIC
	ArithmeticLink.cc
	AssignLink.cc
	DeleteLink.cc
	FoldLink.cc
	FreeLink.cc
	FunctionLink.cc
	PlusLink.cc
	PutLink.cc
	TimesLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(clearbox opencog_atom_types)

TARGET_LINK_LIBRARIES(clearbox
	execution
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS clearbox
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ArithmeticLink.h
	AssignLink.h
	DeleteLink.h
	FoldLink.h
	FreeLink.h
	FunctionLink.h
	PlusLink.h
	PutLink.h
	TimesLink.h
	DESTINATION "include/${PROJECT_NAME}/atoms/reduct"
)
