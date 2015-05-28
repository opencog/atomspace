# Build file for the atomspace benchmark

IF (HAVE_CYTHON)
	INCLUDE_DIRECTORIES (
		${PYTHON_INCLUDE_DIRS}
	)
ENDIF (HAVE_CYTHON)

ADD_EXECUTABLE (atomspace_bm
	AtomSpaceBenchmark.cc
	atomspace_bm.cc
)

IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES (atomspace_bm smob)
ENDIF(HAVE_GUILE)

IF (HAVE_CYTHON)
	TARGET_LINK_LIBRARIES (atomspace_bm
		atomspace_cython
		${PYTHON_LIBRARIES}
	)
ENDIF(HAVE_CYTHON)

TARGET_LINK_LIBRARIES (atomspace_bm
	atomspace
	execution
	clearbox
	${COGUTIL_LIBRARY}
)
