
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (free
	FindUtils.cc
	FreeLink.cc
	FreeVariables.cc
	Quotation.cc
	Replacement.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(free opencog_atom_types)

TARGET_LINK_LIBRARIES(free
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS free EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FindUtils.h
	FreeLink.h
	FreeVariables.h
	Quotation.h
	Replacement.h
	DESTINATION "include/opencog/atoms/free"
)
