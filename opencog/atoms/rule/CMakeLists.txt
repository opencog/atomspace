
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (rule
	RuleLink.cc
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
	RuleLink.h
	DESTINATION "include/opencog/atoms/rule"
)
