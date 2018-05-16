
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (lambda_
	BindLink.cc
	DualLink.cc
	PatternLink.cc
	PatternTerm.cc
	PatternUtils.cc
	Pattern.cc
)

ADD_LIBRARY (lambda
	${DUMMY_CC}
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(lambda_ opencog_atom_types)
ADD_DEPENDENCIES(lambda lambda_)


TARGET_LINK_LIBRARIES(lambda_
	atomspace
	atomutils
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)
TARGET_LINK_LIBRARIES(lambda
	# The library contains constructors. If the linker omits this library
	# because it does not contain explicitly referenced symbols,
	# then the target execution might break in runtime in unpredictable manner.
	${NO_AS_NEEDED}
	lambda_
)

INSTALL (TARGETS lambda_
  DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
INSTALL (TARGETS lambda
  DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	BindLink.h
	DualLink.h
	PatternLink.h
	Pattern.h
	PatternTerm.h
	PatternUtils.h
	DESTINATION "include/opencog/atoms/pattern"
)
