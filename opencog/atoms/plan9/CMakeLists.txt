
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

IF (HAVE_GUILE)
	INCLUDE_DIRECTORIES(${GUILE_INCLUDE_DIR})
ENDIF (HAVE_GUILE)

ADD_LIBRARY (atomplan9
	Nine9P.cc
	NineNode.cc
	SheafHook.cc
	AtomFs.cc
	NineNamespace.cc
	NineServer.cc
)

IF (NOT WIN32)
	TARGET_SOURCES(atomplan9 PRIVATE NineTcpServer.cc)
ENDIF (NOT WIN32)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(atomplan9 opencog_atom_types value)

TARGET_LINK_LIBRARIES(atomplan9
	atomspace
	atombase
	atom_types
	value
	${COGUTIL_LIBRARY}
)

IF (NOT WIN32)
	FIND_PACKAGE(Threads REQUIRED)
	TARGET_LINK_LIBRARIES(atomplan9 ${CMAKE_THREAD_LIBS_INIT})
ENDIF (NOT WIN32)

# SchemeSheafHook (SheafHook.cc, HAVE_GUILE-gated) calls into
# opencog::SchemeEval; as with other atoms/ libraries that touch
# Guile conditionally (see atoms/grounded), we deliberately do not
# link against `smob` here, since `smob` itself links every atom
# type library including this one's dependencies -- linking it back
# would invert the intended build order (atoms/ is built before
# guile/). The undefined SchemeEval symbols are resolved when smob
# is loaded alongside this library at run time.

MESSAGE(STATUS "Compile time defs are: ${COMPILE_TIME_DEFS}")
SET_TARGET_PROPERTIES(atomplan9 PROPERTIES
	INTERFACE_COMPILE_DEFINITIONS "${COMPILE_TIME_DEFS}")

INSTALL (TARGETS atomplan9 EXPORT AtomSpaceTargets
	DESTINATION "${CMAKE_INSTALL_LIBDIR}/opencog"
)

INSTALL (FILES
	Nine9P.h
	NineNode.h
	SheafHook.h
	AtomFs.h
	NineNamespace.h
	NineServer.h
	NineTcpServer.h
	DESTINATION "include/opencog/atoms/plan9"
)
