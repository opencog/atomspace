INCLUDE_DIRECTORIES (
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/persist/gearman
)

LINK_DIRECTORIES (
	${PROJECT_BINARY_DIR}/opencog/atomspace
)

LINK_LIBRARIES (
	smob
	atomspace
)

#start gearman-job-server

ADD_CXXTEST(GearmanUTest)
