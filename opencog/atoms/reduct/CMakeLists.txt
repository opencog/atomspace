
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (clearbox
	AccumulateLink.cc
	ArithmeticLink.cc
	BoolOpLink.cc
	DivideLink.cc
	FoldLink.cc
	MaxLink.cc
	MinLink.cc
	MinusLink.cc
	NumericFunctionLink.cc
	PlusLink.cc
	TimesLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(clearbox opencog_atom_types)

TARGET_LINK_LIBRARIES(clearbox
	atom_types
	atomcore
	atombase
	value
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS clearbox EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	AccumulateLink.h
	ArithmeticLink.h
	BoolOpLink.h
	DivideLink.h
	FoldLink.h
	MaxLink.h
	MinLink.h
	MinusLink.h
	NumericFunctionLink.h
	PlusLink.h
	TimesLink.h
	DESTINATION "include/opencog/atoms/reduct"
)
