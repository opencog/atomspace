
# Build the GearMan based distributed system.
#
ADD_LIBRARY(dist-gearman SHARED
	DistSCM.cc
)

ADD_DEPENDENCIES(dist-gearman
	opencog_atom_types
)

TARGET_LINK_LIBRARIES(dist-gearman
	smob
	atomspace
	${GEARMAN_LIBRARY}
)

INSTALL (TARGETS dist-gearman DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (FILES
	DistSCM.h
	DESTINATION "include/opencog/persist/gearman"
)
