
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (clearbox SHARED
	ArithmeticLink.cc
	DivideLink.cc
	FoldLink.cc
	MinusLink.cc
	PlusLink.cc
	TimesLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(clearbox opencog_atom_types)

TARGET_LINK_LIBRARIES(clearbox
	atombase
	atomcore
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS clearbox
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ArithmeticLink.h
	DivideLink.h
	FoldLink.h
	MinusLink.h
	PlusLink.h
	TimesLink.h
	DESTINATION "include/opencog/atoms/reduct"
)
