
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (rule
	ConclusionOfLink.cc
	PremiseOfLink.cc
	RuleLink.cc
	VardeclOfLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(rule opencog_atom_types)

TARGET_LINK_LIBRARIES(rule
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS rule EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	ConclusionOfLink.h
	PremiseOfLink.h
	RuleLink.h
	VardeclOfLink.h
	DESTINATION "include/opencog/atoms/rule"
)
