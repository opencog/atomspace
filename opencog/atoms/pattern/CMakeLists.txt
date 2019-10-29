
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (lambda
	BindLink.cc
	DualLink.cc
	GetLink.cc
	PatternLink.cc
	PatternTerm.cc
	PatternUtils.cc
	Pattern.cc
	QueryLink.cc
	SatisfactionLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(lambda opencog_atom_types)

TARGET_LINK_LIBRARIES(lambda
	query-engine
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS lambda EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	BindLink.h
	DualLink.h
	GetLink.h
	PatternLink.h
	Pattern.h
	PatternTerm.h
	PatternUtils.h
	QueryLink.h
	SatisfactionLink.h
	DESTINATION "include/opencog/atoms/pattern"
)
