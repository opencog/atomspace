
ADD_SUBDIRECTORY (bind)
ADD_SUBDIRECTORY (execution)
ADD_SUBDIRECTORY (reduct)

INSTALL (FILES
	NumberNode.h
	TypeNode.h
	DESTINATION "include/${PROJECT_NAME}/atoms"
)
