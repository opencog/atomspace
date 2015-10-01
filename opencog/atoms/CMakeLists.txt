
ADD_SUBDIRECTORY (bind)
ADD_SUBDIRECTORY (core)
ADD_SUBDIRECTORY (execution)
ADD_SUBDIRECTORY (reduct)

INSTALL (FILES
	NumberNode.h
	TypeNode.h
	DESTINATION "include/opencog/atoms"
)
