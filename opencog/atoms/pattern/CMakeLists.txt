
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

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(lambda_ opencog_atom_types)

TARGET_LINK_LIBRARIES(lambda_
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

ADD_LIBRARY (lambda dummy)
TARGET_LINK_LIBRARIES(lambda
	lambda_
	query_
	smob_
	atomspace
)

IF (HAVE_CYTHON)
	TARGET_LINK_LIBRARIES(lambda PythonEval)
ENDIF (HAVE_CYTHON)


INSTALL (TARGETS lambda DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (TARGETS lambda_ DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	BindLink.h
	DualLink.h
	PatternLink.h
	Pattern.h
	PatternTerm.h
	PatternUtils.h
	DESTINATION "include/opencog/atoms/pattern"
)
