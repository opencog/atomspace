
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (clearbox
	AccumulateLink.cc
	ArithmeticLink.cc
	DivideLink.cc
	FoldLink.cc
	HeavisideLink.cc
	Log2Link.cc
	MaxLink.cc
	MinLink.cc
	MinusLink.cc
	NumericOutLink.cc
	PlusLink.cc
	PowLink.cc
	RandomNumber.cc
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
	DivideLink.h
	FoldLink.h
	HeavisideLink.h
	Log2Link.h
	MaxLink.h
	MinLink.h
	MinusLink.h
	NumericOut.h
	PlusLink.h
	PowLink.h
	RandomNumber.h
	TimesLink.h
	DESTINATION "include/opencog/atoms/reduct"
)
