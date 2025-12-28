
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (free
	Context.cc
	FindUtils.cc
	FreeLink.cc
	FreeVariables.cc
	Quotation.cc
	QuoteLink.cc
	QuoteReduce.cc
	Replacement.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(free opencog_atom_types)

TARGET_LINK_LIBRARIES(free
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS free EXPORT AtomSpaceTargets
	DESTINATION "${CMAKE_INSTALL_LIBDIR}/opencog"
)

INSTALL (FILES
	Context.h
	FindUtils.h
	FreeLink.h
	FreeVariables.h
	Quotation.h
	QuoteLink.h
	QuoteReduce.h
	Replacement.h
	DESTINATION "include/opencog/atoms/free"
)
