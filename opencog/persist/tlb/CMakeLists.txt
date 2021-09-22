
ADD_LIBRARY (tlb
	TLB
)

TARGET_LINK_LIBRARIES(tlb
	atombase
	atomspace
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS tlb EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	TLB.h
	DESTINATION "include/opencog/persist/tlb"
)
