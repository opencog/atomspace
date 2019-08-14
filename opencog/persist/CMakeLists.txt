
IF (HAVE_GEARMAN AND HAVE_GUILE)
	ADD_SUBDIRECTORY (gearman)
ENDIF(HAVE_GEARMAN AND HAVE_GUILE)

IF (GUILE_FOUND)
	ADD_SUBDIRECTORY (guile)
ENDIF (GUILE_FOUND)

ADD_SUBDIRECTORY (sql)

# ZMQ is unmaintained, and is not compatible with the AtomSpace any
# longer.  Disable the build.
# IF (HAVE_ZMQ)
# 	ADD_SUBDIRECTORY (zmq)
# ENDIF (HAVE_ZMQ)
