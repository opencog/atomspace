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

TARGET_LINK_LIBRARIES (atomspace_bm m
	atomspace
	${COGUTIL_LIBRARY}
)

IF (HAVE_GUILE)
	ADD_EXECUTABLE (profile_bindlink
		profile_bindlink.cc
	)

	TARGET_LINK_LIBRARIES (profile_bindlink m
		atomutils
		attentionbank
		atomspace
		execution
		query
		execution
		clearbox
		${COGUTIL_LIBRARY}
		atomcore
		atomutils
		dl
	)
ENDIF (HAVE_GUILE)
