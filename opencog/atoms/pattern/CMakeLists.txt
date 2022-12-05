
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (pattern
	BindLink.cc
	DualLink.cc
	GetLink.cc
	MeetLink.cc
	PatternJit.cc
	PatternLink.cc
	PatternTerm.cc
	PatternUtils.cc
	Pattern.cc
	QueryLink.cc
	SatisfactionLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(pattern opencog_atom_types)

# TARGET_COMPILE_OPTIONS(pattern PRIVATE -DQDEBUG=1)

TARGET_LINK_LIBRARIES(pattern
	query-engine
	rule
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS pattern EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	BindLink.h
	DualLink.h
	GetLink.h
	MeetLink.h
	PatternLink.h
	Pattern.h
	PatternTerm.h
	PatternUtils.h
	QueryLink.h
	SatisfactionLink.h
	DESTINATION "include/opencog/atoms/pattern"
)
