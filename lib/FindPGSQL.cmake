# Copyright (c) 2008, OpenCog.org (http://opencog.org)
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

# - Try to find the PostgreSQL library; Once done this will define
#
# PGSQL_FOUND - system has the PostgreSQL library
# PGSQL_INCLUDE_DIRS - the PostgreSQL include directory
# PGSQL_LIBRARIES - The libraries needed to use PostgreSQL

# Look for the header file
FIND_PATH(PGSQL_INCLUDE_DIR libpq-fe.h
	/usr/include
	/usr/local/include
	/usr/include/postgresql
	/usr/local/include/postgresql
)

# Look for the library
FIND_LIBRARY(PGSQL_LIBRARY
	NAMES
		pq
	PATHS
		/usr/lib
		/usr/local/lib)

FIND_LIBRARY(GSASL_LIBRARY
	NAMES
		gsasl
	PATHS
		/usr/lib
		/usr/local/lib)

# Copy the results to the output variables.
IF (PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)
	SET(PGSQL_FOUND 1)
	SET(PGSQL_LIBRARIES ${PGSQL_LIBRARY}
			com_err crypto ssl ldap krb5 gssapi_krb5 ${GSASL_LIBRARY})
	SET(PGSQL_INCLUDE_DIRS ${PGSQL_INCLUDE_DIR})
ELSE (PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)
	SET(PGSQL_FOUND 0)
	SET(PGSQL_LIBRARIES)
	SET(PGSQL_INCLUDE_DIRS)
ENDIF (PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)

# Report the results.
IF (NOT PGSQL_FOUND)
	SET(PGSQL_DIR_MESSAGE "PostgreSQL was not found. Make sure PGSQL_LIBRARY and PGSQL_INCLUDE_DIR are set.")
	IF (NOT PGSQL_FIND_QUIETLY)
		MESSAGE(STATUS "${PGSQL_DIR_MESSAGE}")
	ELSE (NOT PGSQL_FIND_QUIETLY)
		IF (PGSQL_FIND_REQUIRED)
			MESSAGE(FATAL_ERROR "${PGSQL_DIR_MESSAGE}")
		ENDIF (PGSQL_FIND_REQUIRED)
	ENDIF (NOT PGSQL_FIND_QUIETLY)
ENDIF (NOT PGSQL_FOUND)

MARK_AS_ADVANCED(PGSQL_INCLUDE_DIRS)
MARK_AS_ADVANCED(PGSQL_LIBRARIES)
