
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (lambda
	BindLink.cc
	DualLink.cc
	PatternLink.cc
	PatternTerm.cc
	PatternUtils.cc
	Pattern.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(lambda opencog_atom_types)

TARGET_LINK_LIBRARIES(lambda
	atomspace
	atomutils
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

IF (CYGWIN)
	INSTALL (TARGETS lambda
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ELSE (CYGWIN)
	INSTALL (TARGETS lambda
		DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)
ENDIF (CYGWIN)

INSTALL (FILES
	BindLink.h
	DualLink.h
	PatternLink.h
	Pattern.h
	PatternTerm.h
	PatternUtils.h
	DESTINATION "include/opencog/atoms/pattern"
)
