# Copyright (c) 2008, OpenCog.org (http://opencog.org)
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

# - Try to find the PostgreSQL library; Once done this will define
#
# PGSQL_FOUND - system has the PostgreSQL library and required dependencies
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
FIND_LIBRARY(PGSQL_LIBRARY NAMES pq)

FIND_LIBRARY(GSASL_LIBRARY
	NAMES
		libgsasl.so.7
	PATHS
		/usr/lib
		/usr/local/lib)

# SASL, kerberos, gssapi are all optional.
IF (GSASL_LIBRARY STREQUAL "GSASL_LIBRARY-NOTFOUND")
	SET(GSASL_LIBRARY)
ENDIF (GSASL_LIBRARY STREQUAL "GSASL_LIBRARY-NOTFOUND")

FIND_LIBRARY(KRB5_LIBRARY
	NAMES
		krb5
	PATHS
		/usr/lib
		/usr/local/lib
		/usr/lib/x86_64-linux-gnu/
		/usr/lib/x86_64-linux-gnu/mit-krb5/
		)

IF (KRB5_LIBRARY STREQUAL "KRB5_LIBRARY-NOTFOUND")
	SET(KRB5_LIBRARY)
ENDIF (KRB5_LIBRARY STREQUAL "KRB5_LIBRARY-NOTFOUND")

FIND_LIBRARY(GSSAPI_KRB5_LIBRARY
	NAMES
		gssapi_krb5
	PATHS
		/usr/lib
		/usr/local/lib
		/usr/lib/x86_64-linux-gnu/
		/usr/lib/x86_64-linux-gnu/mit-krb5/
		)

IF (GSSAPI_KRB5_LIBRARY STREQUAL "GSSAPI_KRB5_LIBRARY-NOTFOUND")
	SET(GSSAPI_KRB5_LIBRARY)
ENDIF (GSSAPI_KRB5_LIBRARY STREQUAL "GSSAPI_KRB5_LIBRARY-NOTFOUND")

# Copy the results to the output variables.
IF (PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)
	# AND KRB5_LIBRARY AND GSSAPI_KRB5_LIBRARY AND GSASL_LIBRARY)

	SET(PGSQL_FOUND 1)
	SET(PGSQL_LIBRARIES ${PGSQL_LIBRARY} )
			# com_err crypto ssl ldap
			# ${KRB5_LIBRARY} ${GSSAPI_KRB5_LIBRARY} ${GSASL_LIBRARY})
	SET(PGSQL_INCLUDE_DIRS ${PGSQL_INCLUDE_DIR})
ELSE (PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)
	#  AND KRB5_LIBRARY AND GSSAPI_KRB5_LIBRARY AND GSASL_LIBRARY)
	SET(PGSQL_FOUND 0)
	SET(PGSQL_LIBRARIES)
	SET(PGSQL_INCLUDE_DIRS)
ENDIF (PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)
	# AND KRB5_LIBRARY AND GSSAPI_KRB5_LIBRARY AND GSASL_LIBRARY)

# Report the results.
IF (NOT PGSQL_FOUND)
	# IF (NOT GSASL_LIBRARY)
	#	SET(PGSQL_DIR_MESSAGE "PostgreSQL missing required library. PostgreSQL support requires the Gnu SASL library.")
	# ELSEIF (NOT KRB5_LIBRARY OR NOT GSSAPI_KRB5_LIBRARY)
	#	SET(PGSQL_DIR_MESSAGE "PostgreSQL missing required library. PostgreSQL support requires the MIT Kerberos library.")
	# ELSE (NOT GSASL_LIBRARY)
	#	SET(PGSQL_DIR_MESSAGE "PostgreSQL was not found. Make sure PGSQL_LIBRARY and PGSQL_INCLUDE_DIR are set.")
	# ENDIF (NOT GSASL_LIBRARY)
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
