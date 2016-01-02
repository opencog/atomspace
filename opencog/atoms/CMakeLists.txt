
ADD_SUBDIRECTORY (base)
ADD_SUBDIRECTORY (core)
ADD_SUBDIRECTORY (execution)
ADD_SUBDIRECTORY (pattern)
ADD_SUBDIRECTORY (reduct)

INSTALL (FILES
	NumberNode.h
	TypeNode.h
	DESTINATION "include/opencog/atoms"
)
