
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (clearbox SHARED
	FoldLink.cc
	FreeLink.cc
	FunctionLink.cc
	PlusLink.cc
	TimesLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(clearbox opencog_atom_types)

TARGET_LINK_LIBRARIES(clearbox
	${COGUTIL_LIBRARY}
)

IF (CYGWIN)
	INSTALL (TARGETS clearbox
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ELSE (CYGWIN)
	INSTALL (TARGETS clearbox
		LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (CYGWIN)

INSTALL (FILES
	FoldLink.h
	FreeLink.h
	FunctionLink.h
	PlusLink.h
	TimesLink.h
	DESTINATION "include/${PROJECT_NAME}/atoms/reduct"
)
