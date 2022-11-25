
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

ADD_GUILE_EXTENSION(SCM_CONFIG dist-gearman "opencog-ext-path-dist-gearman")

ADD_GUILE_MODULE (FILES
	opencog/dist-gearman.scm
	DEPENDS dist-gearman
#	COMPILE
)

INSTALL (TARGETS dist-gearman 
    EXPORT AtomSpaceTargets 
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
