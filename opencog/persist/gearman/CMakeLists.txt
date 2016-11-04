
# Build the GearMan based distributed system.
#
ADD_LIBRARY(dist-gearman
	DistSCM.cc
)

TARGET_LINK_LIBRARIES(dist-gearman
	smob
	atomspace
	${GEARMAN_LIBRARY}
)

INSTALL (TARGETS dist-gearman DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
