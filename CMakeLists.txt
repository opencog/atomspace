#
# Master AtomSpace CMake file.
#
# General organization:
# -- check for different compilers, OS'es
# -- search for various required & optional libraries/tools
# -- decide what to build based on above results.
# -- configure various config files.
# -- print pretty summary
#
# cogutils already requires 2.8.12.2, so may as well ask for that.
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12.2)
IF (COMMAND CMAKE_POLICY)
	CMAKE_POLICY(SET CMP0003 NEW)
ENDIF (COMMAND CMAKE_POLICY)

IF(CMAKE_VERSION VERSION_GREATER 3.0.2)
	CMAKE_POLICY(SET CMP0037 OLD)
ENDIF(CMAKE_VERSION VERSION_GREATER 3.0.2)

PROJECT(atomspace)

# ----------------------------------------------------------
# User-modifiable options. Feel free to change these!
#
# Uncomment to be in Release mode [default].
# SET(CMAKE_BUILD_TYPE Release)

# Uncomment to build in debug mode.
# SET(CMAKE_BUILD_TYPE Debug)

# Uncomment to be in coverage testing mode.
# SET(CMAKE_BUILD_TYPE Coverage)

# Uncomment to build in profile mode.
# SET(CMAKE_BUILD_TYPE Profile)

# Uncomment to build in release mode with debug information.
# SET(CMAKE_BUILD_TYPE RelWithDebInfo)

# default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
                -DPROJECT_BINARY_DIR="${CMAKE_BINARY_DIR}")

# ===============================================================
# Detect different compilers and OS'es, tweak flags as necessary.

# The default case for non-profile builds is to use shared libraries. So don't
# use explicit SHARED in the ADD_LIBRARY calls in CMakeLists.txt instances or
# this flag won't work since it only affects the default.
IF (CMAKE_BUILD_TYPE STREQUAL "Profile")
	SET(BUILD_SHARED_LIBS OFF)
ELSE (CMAKE_BUILD_TYPE STREQUAL "Profile")
	SET(BUILD_SHARED_LIBS ON)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "Profile")

# ----------------------------------------------------
# Do the windows build.

IF (WIN32)
	ADD_DEFINITIONS(-DWIN32)
	IF (CYGWIN)
		ADD_DEFINITIONS(-DCYGWIN)
	#   SET(WIN32 1)
	ENDIF (CYGWIN)

	# It is "not unix" when the code is compiled under windows but not
	# under cygwin
	IF (NOT UNIX)
		ADD_DEFINITIONS(-DWIN32_NOT_UNIX)
		FIND_PACKAGE(PThreads REQUIRED)
		FIND_PACKAGE(STLPort REQUIRED)
		INCLUDE_DIRECTORIES("${CMAKE_SOURCE_DIR}/include/win32" ${PTHREADS_INCLUDE_DIR} ${STLPORT_INCLUDE_DIR})
		LINK_LIBRARIES(${PTHREADS_LIBRARY} ${STLPORT_LIBRARIES})
		ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
	ELSE (NOT UNIX)
		LINK_LIBRARIES(pthread)
		IF (CYGWIN)
			SET(CMAKE_SHARED_LINKER_FLAGS "-Wl")
		ELSE (CYGWIN)
			SET(CMAKE_SHARED_LINKER_FLAGS "-Wl,--enable-new-dtags")
		ENDIF (CYGWIN)
	ENDIF (NOT UNIX)
ENDIF (WIN32)

# ===============================================================
# Check for existance of various required, optional packages.
# Listed in alphabetical order, more or less.
# CogUtil must come first, because it supplies various FindXXX macros.

# Add the 'lib' dir to cmake's module search path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/lib/")

# Cogutil
FIND_PACKAGE(CogUtil CONFIG)
IF (COGUTIL_FOUND)
	MESSAGE(STATUS "CogUtil version ${COGUTIL_VERSION} found.")
	ADD_DEFINITIONS(-DHAVE_COGUTIL)
	SET(HAVE_COGUTIL 1)
ELSE (COGUTIL_FOUND)
	MESSAGE(FATAL_ERROR "CogUtil missing: it is needed!")
ENDIF (COGUTIL_FOUND)

# add the 'cmake' directory from cogutil to search path
list(APPEND CMAKE_MODULE_PATH  ${COGUTIL_DATA_DIR}/cmake)
include(OpenCogGccOptions)
include(Summary)

# ----------------------------------------------------------
# Check for boost. We need dynamic-linked, threaded libs by default.
SET(Boost_USE_STATIC_LIBS OFF)
SET(Boost_USE_MULTITHREADED ON)
SET(MIN_BOOST 1.46)

# Required boost packages
FIND_PACKAGE(Boost ${MIN_BOOST} REQUIRED COMPONENTS date_time filesystem serialization system thread)

IF(Boost_FOUND)
	SET(Boost_FOUND_SAVE 1)
ELSE(Boost_FOUND)
	MESSAGE(FATAL_ERROR "Boost ${MIN_BOOST} or newer is needed to build OpenCog!")
ENDIF(Boost_FOUND)

# Atomspace won't compile with Boost 1.51, some kind of conflict with
# hash functions, see github bugs 1 and 36
IF(105100 EQUAL ${Boost_VERSION})
	MESSAGE(FATAL_ERROR "Boost version 1.51 will not work with AtomSpace.  Please use a different version.")
ENDIF(105100 EQUAL ${Boost_VERSION})

MESSAGE(STATUS "Boost version ${Boost_VERSION} found.")

# Arghhh. Except cmake is treating above as required, not optional. #$%**&
IF(Boost_FOUND_SAVE)
	SET(Boost_FOUND 1)
ENDIF(Boost_FOUND_SAVE)

# ----------------------------------------------------------
# Needed for unit tests.

FIND_PACKAGE(Cxxtest)
IF (CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest found.")
ELSE (CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest missing: needed for unit tests.")
ENDIF (CXXTEST_FOUND)

# ----------------------------------------------------------
# Optional, unused distributed processing framework.

FIND_PACKAGE(LibGearman)
ADD_DEFINITIONS(-DHAVE_GEARMAN)
IF(GEARMAN_FOUND)
	SET(HAVE_GEARMAN 1)
	SET(GEARMAN_DIR_MESSAGE "Gearman found.")
ELSE (GEARMAN_FOUND)
	SET(HAVE_GEARMAN 0)
	SET(GEARMAN_DIR_MESSAGE "Gearman not found.")
ENDIF(GEARMAN_FOUND)
MESSAGE(STATUS "${GEARMAN_DIR_MESSAGE}")

# ----------------------------------------------------------
# This is required for Guile
FIND_LIBRARY(GMP_LIBRARY gmp)
FIND_PATH(GMP_INCLUDE_DIR gmp.h)

# Gnu Guile scheme interpreter. Mandatory
# Version 2.2.2 is needed for mrs io ports, compilation, atomics, nlp
FIND_PACKAGE(Guile 2.2.2)
IF (GUILE_FOUND AND GMP_LIBRARY AND GMP_INCLUDE_DIR)
	ADD_DEFINITIONS(-DHAVE_GUILE)
	SET(HAVE_GUILE 1)
	INCLUDE_DIRECTORIES(${GUILE_INCLUDE_DIR})
ELSE (GUILE_FOUND AND GMP_LIBRARY AND GMP_INCLUDE_DIR)
	SET(GUILE_DIR_MESSAGE "Guile was not found; the scheme shell will not be built.\nTo over-ride, make sure GUILE_LIBRARIES and GUILE_INCLUDE_DIRS are set.")
	MESSAGE(FATAL_ERROR "${GUILE_DIR_MESSAGE}")
ENDIF (GUILE_FOUND AND GMP_LIBRARY AND GMP_INCLUDE_DIR)

# ----------------------------------------------------------
# Google Protobuf library
# The protocol buffer compiler is needed for ZMQ-based persistence.
FIND_PACKAGE(Protobuf)
IF (PROTOBUF_FOUND
		AND PROTOBUF_LIBRARY
		AND PROTOBUF_INCLUDE_DIR
		AND PROTOBUF_PROTOC_EXECUTABLE)

	ADD_DEFINITIONS(-DHAVE_PROTOBUF)
	SET(HAVE_PROTOBUF 1)
	SET(PROTOBUF_DIR_MESSAGE "Protobuf was found.")

ELSE (PROTOBUF_FOUND
		AND PROTOBUF_LIBRARY
		AND PROTOBUF_INCLUDE_DIR
		AND PROTOBUF_PROTOC_EXECUTABLE)

	# If we are here, then we are missing either the libraries, or the
	# compiler. React appropriately.
	IF (PROTOBUF_PROTOC_EXECUTABLE)
		SET(PROTOBUF_DIR_MESSAGE "Protobuf libraries were not found; these are needed to link the ZMQ-based persistence backend.\n	To over-ride, make sure PROTOBUF_LIBRARIES and PROTOBUF_INCLUDE_DIRS are set.")
	ELSE (PROTOBUF_PROTOC_EXECUTABLE)
		SET(PROTOBUF_DIR_MESSAGE "Protobuf compiler was not found; the ZMQ-based persistence backend can't be built without it.\n	To over-ride, make sure PROTOBUF_PROTOC_EXECUTABLE is set.")
	ENDIF (PROTOBUF_PROTOC_EXECUTABLE)

ENDIF (PROTOBUF_FOUND
		AND PROTOBUF_LIBRARY
		AND PROTOBUF_INCLUDE_DIR
		AND PROTOBUF_PROTOC_EXECUTABLE)

MESSAGE(STATUS "${PROTOBUF_DIR_MESSAGE}")

# ----------------------------------------------------------
# Python and Cython
#
# NOTE: Python interpreter is needed for runing python unit tests,
# and for running the FindCython module.
#
# Search for Python3 first, and use that, if found. Else use Python2.
# To use Python2 only from the build directory run the following
# rm CMakeCache.txt && cmake -DCMAKE_DISABLE_FIND_PACKAGE_Python3Interp=TRUE ..

FIND_PACKAGE(Python3Interp)
IF (3.4.0 VERSION_LESS "${PYTHON3_VERSION_STRING}")
	SET (HAVE_PY_INTERP 1)
	SET (PYTHON_VER python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR})
	MESSAGE(STATUS "Python ${PYTHON3_VERSION_STRING} interpreter found.")
ENDIF()

IF (NOT HAVE_PY_INTERP)
	FIND_PACKAGE(PythonInterp)
	IF (2.7.0 VERSION_LESS ${PYTHON_VERSION_STRING})
		SET (HAVE_PY_INTERP 1)
		SET (PYTHON_VER python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR})
		MESSAGE(STATUS "Python ${PYTHON_VERSION_STRING} interpreter found.")
	ENDIF()
ENDIF()

FIND_PACKAGE(PythonLibs)
IF (PYTHONLIBS_FOUND AND
     ((PYTHON3INTERP_FOUND AND 3.4.0 VERSION_LESS ${PYTHONLIBS_VERSION_STRING})
     OR
     (2.7.0 VERSION_LESS ${PYTHONLIBS_VERSION_STRING})))
	SET (HAVE_PY_LIBS 1)
	MESSAGE(STATUS "Python ${PYTHONLIBS_VERSION_STRING} libraries found.")
ELSE()
	MESSAGE(STATUS "Python libraries NOT found.")
ENDIF()

# Cython is used to generate python bindings.
IF(HAVE_PY_INTERP)
	FIND_PACKAGE(Cython 0.23.0)

	IF (CYTHON_FOUND AND HAVE_PY_LIBS)
		ADD_DEFINITIONS(-DHAVE_CYTHON)
		SET(HAVE_CYTHON 1)

		# Find python destination dir for python bindings
		# because it may differ on each operating system.
		EXECUTE_PROCESS(
			COMMAND ${PYTHON_EXECUTABLE} -c "from distutils.sysconfig import *;print(get_python_lib())"
			OUTPUT_VARIABLE PYTHON_DEST
		)

		# Chop off leading /usr
		STRING(REPLACE "/usr" ${CMAKE_INSTALL_PREFIX} PYTHON_DEST "${PYTHON_DEST}")
		# Replace new line at end
		STRING(REPLACE "\n" "" PYTHON_DEST "${PYTHON_DEST}")
		IF ("${PYTHON_DEST}" STREQUAL "")
			MESSAGE(FATAL_ERROR "Python destination dir not found")
		ELSE ("${PYTHON_DEST}" STREQUAL "")
			MESSAGE(STATUS "Python destination dir found: ${PYTHON_DEST}" )
		ENDIF ("${PYTHON_DEST}" STREQUAL "")

		# thunk
		SET(PYTHON_ROOT "${PYTHON_DEST}")
		SET(PYTHON_DEST "${PYTHON_DEST}/opencog")

		MESSAGE(STATUS
			"Python install dir: ${PYTHON_DEST}" )

	ELSE (CYTHON_FOUND AND HAVE_PY_LIBS)
		IF(NOT CYTHON_FOUND)
			MESSAGE(STATUS "Cython executable not found.")
		ENDIF(NOT CYTHON_FOUND)
	ENDIF (CYTHON_FOUND AND HAVE_PY_LIBS)

	# Nosetests will find and automatically run python tests.
	IF (PYTHON3INTERP_FOUND AND 3.4.0 VERSION_LESS ${PYTHONLIBS_VERSION_STRING})
		FIND_PROGRAM(NOSETESTS_EXECUTABLE nosetests3)
	ELSE ()
		FIND_PROGRAM(NOSETESTS_EXECUTABLE nosetests-2.7)
	ENDIF ()
	IF (NOT NOSETESTS_EXECUTABLE)
		MESSAGE(STATUS "nosetests not found: needed for python tests")
	ENDIF ()
	IF (NOSETESTS_EXECUTABLE AND CYTHON_FOUND AND HAVE_PY_LIBS)
		SET(HAVE_NOSETESTS 1)
		MESSAGE(STATUS "Using nosetests executable " ${NOSETESTS_EXECUTABLE})
	ENDIF (NOSETESTS_EXECUTABLE AND CYTHON_FOUND AND HAVE_PY_LIBS)
ENDIF(HAVE_PY_INTERP)

# ----------------------------------------------------------
# Glasgow Haskell compiler

FIND_PACKAGE(Stack)
IF (STACK_FOUND)
	ADD_DEFINITIONS(-DHAVE_STACK)
	SET(HAVE_STACK 1)
ENDIF (STACK_FOUND)

# ----------------------------------------------------------
# Optional/deprecated ODBC.

FIND_PACKAGE(UnixODBC QUIET)
IF (UnixODBC_FOUND)
	ADD_DEFINITIONS(-DHAVE_SQL_STORAGE)
	SET(HAVE_SQL_STORAGE 1)
	ADD_DEFINITIONS(-DHAVE_ODBC_STORAGE)
	SET(ODBC_FOUND 1)
	SET(ODBC_INCLUDE_DIRS ${UnixODBC_INCLUDE_DIRS})
	SET(ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
	SET(ODBC_DIR_MESSAGE "UnixODBC was found.")
ELSE (UnixODBC_FOUND)
	SET(ODBC_FOUND 0)
	SET(ODBC_DIR_MESSAGE "UnixODBC was not found; ODBC persistence subsystem will not be built.\nTo over-ride, make sure UnixODBC_LIBRARIES and UnixODBC_INCLUDE_DIRS are set.")
ENDIF (UnixODBC_FOUND)
MESSAGE(STATUS "${ODBC_DIR_MESSAGE}")

# ----------------------------------------------------------
# Mandatory Postgres

FIND_PACKAGE(PGSQL 9.5 REQUIRED)
IF (PGSQL_FOUND)
	ADD_DEFINITIONS(-DHAVE_SQL_STORAGE)
	SET(HAVE_SQL_STORAGE 1)
	ADD_DEFINITIONS(-DHAVE_PGSQL_STORAGE)
	SET(HAVE_PGSQL_STORAGE 1)
	SET(PGSQL_FOUND 1)
	SET(PGSQL_DIR_MESSAGE "PostgresSQL ${PGSQL_VERSION_STRING} was found.")
ELSE (PGSQL_FOUND)
	SET(PGSQL_FOUND 0)
	SET(PGSQL_DIR_MESSAGE "PostgresSQL was not found; PGSQL persistence subsystem will not be built.\nTo over-ride, make sure PGSQL_LIBRARIES and PGSQL_INCLUDE_DIRS are set.")
ENDIF (PGSQL_FOUND)
MESSAGE(STATUS "${PGSQL_DIR_MESSAGE}")

# ----------------------------------------------------------
# Optional, currently needed only to hush up DRD in util/Logger.cc
FIND_PACKAGE(VALGRIND)
IF (VALGRIND_FOUND)
	MESSAGE(STATUS "VALGRIND was found.")
	IF (VALGRIND_INCLUDE_DIR)
		MESSAGE(STATUS "VALGRIND devel headers found.")
		ADD_DEFINITIONS(-DHAVE_VALGRIND)
	ELSE (VALGRIND_INCLUDE_DIR)
		MESSAGE(STATUS "VALGRIND devel headers NOT FOUND: needed for thread debugging.")
	ENDIF (VALGRIND_INCLUDE_DIR)
ELSE (VALGRIND_FOUND)
	MESSAGE(STATUS "VALGRIND missing: needed for thread debugging.")
ENDIF (VALGRIND_FOUND)

# ----------------------------------------------------------
# Optional and almost obsolete...
# Remove after the REST interfaces get fixed.
# ZeroMQ
FIND_PACKAGE(ZMQ 3.2.4)
IF (ZMQPP_FOUND AND ZMQ_LIBRARY)
	ADD_DEFINITIONS(-DHAVE_ZMQ)
	SET(HAVE_ZMQLIB 1)
	SET(ZMQ_DIR_MESSAGE "ZeroMQ was found.")
ELSE (ZMQPP_FOUND AND ZMQ_LIBRARY)
	SET(ZMQ_DIR_MESSAGE "ZeroMQ library or its C++ header file was not found; \nTo over-ride, make sure that the environment variable ZMQ_LIBRARY is set.\nInstallation instructions: http://zeromq.org/intro:get-the-software")
ENDIF (ZMQPP_FOUND AND ZMQ_LIBRARY)
MESSAGE(STATUS "${ZMQ_DIR_MESSAGE}")

# ===============================================================
# Get atomspace version

# NOTE: This is the official semantic-version, as it is derived from
# a version-control independent means of declaring versioning.
#
# TODO: Once CMAKE_MINIMUM_REQUIRED >= 3.0.2 check use of policy
# CMP0048.
#
FILE(READ "${CMAKE_SOURCE_DIR}/opencog/atomspace/version.h" _ATOMSPACE_H_CONTENTS)
STRING(REGEX MATCH
	"#define ATOMSPACE_VERSION_STRING \"([0-9]+[.0-9]+[.0-9]+)\""
        _ "${_ATOMSPACE_H_CONTENTS}")
SET(SEMANTIC_VERSION ${CMAKE_MATCH_1})

# ===================================================================
# Global includes

# -------------------------------------------------
# Include configuration.

# Set default include paths.
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR} ${CMAKE_BINARY_DIR}
	${Boost_INCLUDE_DIRS} ${COGUTIL_INCLUDE_DIR})

# Macros that define how atom types get declared.
INCLUDE("${CMAKE_SOURCE_DIR}/cmake/OpenCogMacros.cmake")
INCLUDE("${CMAKE_SOURCE_DIR}/cmake/OpenCogFunctions.cmake")

# -------------------------------------------------
# Library configuration

# Small hack to handle unixes that use "/usr/lib64" instead of
# "/usr/lib" as the default lib path on 64 bit archs.
#
IF (NOT DEFINED LIB_DIR_SUFFIX)
	EXECUTE_PROCESS(COMMAND ${CMAKE_CXX_COMPILER} -print-search-dirs OUTPUT_VARIABLE PRINT_SEARCH_DIRS_OUTPUT)
	STRING(REGEX MATCH "\r?\nlibraries:.*\r?\n" COMPILER_LIB_SEARCH_DIRS ${PRINT_SEARCH_DIRS_OUTPUT})
	IF (NOT ${COMPILER_LIB_SEARCH_DIRS} STREQUAL "")
		STRING(REGEX MATCH "/lib64/:|/lib64:|/lib64\n" HAS_LIB64 ${COMPILER_LIB_SEARCH_DIRS})
		IF (NOT ${HAS_LIB64} STREQUAL "")
			SET(LIB_DIR_SUFFIX "64")
		ENDIF (NOT ${HAS_LIB64} STREQUAL "")
	ENDIF (NOT ${COMPILER_LIB_SEARCH_DIRS} STREQUAL "")
ENDIF (NOT DEFINED LIB_DIR_SUFFIX)

# RPATH handling (see https://cmake.org/Wiki/CMake_RPATH_handling)
# Note: RPATH only supported under Linux!
SET(CMAKE_SKIP_BUILD_RPATH	FALSE)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
SET(CMAKE_INSTALL_RPATH
	"${CMAKE_INSTALL_PREFIX}/lib/opencog"
	"${PYTHON_DEST}")

SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# -------------------------------------------------
# Install configuration

# Only list install files that have actually changed.
SET(CMAKE_INSTALL_MESSAGE "LAZY")

# Set confdir and datadir.
IF (NOT DEFINED CONFDIR)
	SET (CONFDIR "${CMAKE_INSTALL_PREFIX}/etc")
ENDIF (NOT DEFINED CONFDIR)

IF (NOT DEFINED DATADIR)
	SET (DATADIR "${CMAKE_INSTALL_PREFIX}/share/opencog")
ENDIF (NOT DEFINED DATADIR)

ADD_DEFINITIONS(-DCONFDIR="${CONFDIR}")
ADD_DEFINITIONS(-DDATADIR="${DATADIR}")

# (re?)define MAN_INSTALL_DIR
SET (MAN_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/share/man")

# ==========================================================
# Decide what to build, based on the packages found.

IF(HAVE_COGUTIL)
	SET(HAVE_ATOMSPACE 1)
ENDIF(HAVE_COGUTIL)

# Both protobuf and also zmq are needed to build ZMQ API.
IF(HAVE_PROTOBUF AND HAVE_ZMQLIB)
	SET(HAVE_ZMQ 1)
ENDIF(HAVE_PROTOBUF AND HAVE_ZMQLIB)

ADD_SUBDIRECTORY(cmake)
ADD_SUBDIRECTORY(opencog)
ADD_SUBDIRECTORY(lib)

IF (CXXTEST_FOUND)
	ADD_CUSTOM_TARGET(tests)
	ADD_SUBDIRECTORY(tests EXCLUDE_FROM_ALL)
	IF (CMAKE_BUILD_TYPE STREQUAL "Coverage")
		# doing coverage stuff while running tests if this is the Coverage build
		ADD_CUSTOM_TARGET(covtest
			# TODO lcov should be found by cmake first
			# TODO set it up so that we can pick to run coverage per test, or
			# combined across all tests (the latter is MUCH faster). Use a define?
			# There is coverage specific stuff in AddCxxTest.cmake now...
			# -
			WORKING_DIRECTORY tests
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
			COMMENT "Running tests with coverage..."
		)
		ADD_CUSTOM_TARGET(test
			DEPENDS covtest
			WORKING_DIRECTORY ./
			# This script combines the coverage analysis of each test,
			# then creates html in tests/lcov
			# COMMAND genhtml -o ../lcov -t "All AtomSpace unit tests" *.info
			COMMAND ${PROJECT_SOURCE_DIR}/scripts/combine_lcov.sh
			COMMENT "Generating lcov report..."
		)
	ELSE (CMAKE_BUILD_TYPE STREQUAL "Coverage")
		# Coverage is disabled; test normally
		ADD_CUSTOM_TARGET(test
			DEPENDS tests
			WORKING_DIRECTORY tests
			# COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process --output-on-failure $(ARGS)
			COMMENT "Running tests..."
		)
	ENDIF (CMAKE_BUILD_TYPE STREQUAL "Coverage")
	ADD_CUSTOM_TARGET(test_python
		WORKING_DIRECTORY tests/cython
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Python and Cython tests..."
	)
	ADD_CUSTOM_TARGET(test_guile
		WORKING_DIRECTORY tests/scm
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Guile and Python-bridge tests..."
	)
	ADD_CUSTOM_TARGET(test_query
		DEPENDS tests/query
		WORKING_DIRECTORY tests/query
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running pattern matcher tests..."
	)
ENDIF (CXXTEST_FOUND)

ADD_SUBDIRECTORY(examples EXCLUDE_FROM_ALL)

IF (NOT WIN32)
	ADD_CUSTOM_TARGET (examples
		# using CMAKE_BUILD_TOOL results in teh cryptic error message
		# warning: jobserver unavailable: using -j1.  Add `+' to parent make rule.
		# This is because make doesn't know how to pass jobserver args to
		# the submake.  So, instead, just use $(MAKE) (with round parens)
		# -- that will do the right thing.
		# COMMAND ${CMAKE_BUILD_TOOL}
		COMMAND $(MAKE)
		WORKING_DIRECTORY examples
		COMMENT "Building examples"
	)
ENDIF (NOT WIN32)

ADD_CUSTOM_TARGET(cscope
	COMMAND find opencog examples tests -name '*.cc' -o -name '*.h' -o -name '*.cxxtest' -o -name '*.scm' > ${CMAKE_SOURCE_DIR}/cscope.files
	COMMAND cscope -b
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	COMMENT "Generating CScope database"
)

# ===============================================================
# Packaging
## Architecture the package is for.
## TODO: Will give error on non debian distros, fix it.
EXECUTE_PROCESS(COMMAND  dpkg --print-architecture
	OUTPUT_VARIABLE PACKAGE_ARCHITECTURE
	OUTPUT_STRIP_TRAILING_WHITESPACE)
STRING(TIMESTAMP UTC_DATE %Y%m%d UTC)
# If 'sudo make install' is run before 'make package', then install_manifest.txt
# will be owned by root. Creating the file during configuration stage ensures
# that is owned by the builder thus avoiding 'Permission denied' error when
# packaging
FILE(WRITE "${PROJECT_BINARY_DIR}/install_manifest.txt")

## Cpack configuration
SET(CPACK_GENERATOR "DEB")
SET(CPACK_PACKAGE_CONTACT "opencog@googlegroups.com")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
SET(CPACK_PACKAGE_DIRECTORY "${CMAKE_BINARY_DIR}/packages")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The OpenCog AtomSpace")
SET(CPACK_PACKAGE_NAME "atomspace-dev")
SET(CPACK_PACKAGE_VENDOR "opencog.org")
SET(CPACK_PACKAGE_VERSION "${SEMANTIC_VERSION}-${UTC_DATE}")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
SET(CPACK_PACKAGE_FILE_NAME
	"${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}_${PACKAGE_ARCHITECTURE}")
SET(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local")

## Debian specific configurations
SET(DEPENDENCY_LIST
	"guile-2.2-dev (>= 2.2.2)"
	"python-dev (>= 2.7.5)"
	"libzmq3-dev (>= 4.0.4)"
	"libboost-date-time-dev (>= ${MIN_BOOST})"
	"libboost-filesystem-dev (>= ${MIN_BOOST})"
	"libboost-program-options-dev (>= ${MIN_BOOST})"
	"libboost-regex-dev (>= ${MIN_BOOST})"
	"libboost-serialization-dev (>= ${MIN_BOOST})"
	"libboost-thread-dev (>= ${MIN_BOOST})"
	"libboost-system-dev (>= ${MIN_BOOST})"
	"libboost-random-dev (>= ${MIN_BOOST})"
	"libstdc++6 (>= 4.7)"
	"libgearman-dev (>=1.0.6)"
	"libpq-dev (>=9.3.24)"
	"libcogutil-dev (>= 2.0.2)"
)

STRING(REPLACE ";" ", " MAIN_DEPENDENCIES "${DEPENDENCY_LIST}")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "${MAIN_DEPENDENCIES}")
SET(CPACK_DEBIAN_PACKAGE_SECTION "libdevel")
SET(CPACK_DEBIAN_PACKAGE_HOMEPAGE "http://opencog.org")
INCLUDE(CPack)

# ===================================================================
# Documentation.
FIND_PACKAGE(Doxygen)
ADD_SUBDIRECTORY(doc EXCLUDE_FROM_ALL)

# ===================================================================
# Show a summary of what we found, what we will do.

SUMMARY_ADD("AtomSpace" "A weighted and typed graph database" HAVE_ATOMSPACE)
SUMMARY_ADD("Doxygen" "Code documentation" DOXYGEN_FOUND)
SUMMARY_ADD("Gearman" "Distributed processing capability" HAVE_GEARMAN)
SUMMARY_ADD("Haskell bindings" "Haskell bindings" HAVE_STACK)
SUMMARY_ADD("Python bindings" "Python (cython) bindings" HAVE_CYTHON)
SUMMARY_ADD("Python tests" "Python bindings nose tests" HAVE_NOSETESTS)
SUMMARY_ADD("Scheme bindings" "Scheme bindings and shell" HAVE_GUILE)
SUMMARY_ADD("SQL ODBC bindings" "Save/Restore of AtomSpace to database via ODBC" ODBC_FOUND)
SUMMARY_ADD("SQL Postgres bindings" "Save/Restore of AtomSpace to Postgres database" PGSQL_FOUND)
SUMMARY_ADD("ZeroMQ persistence" "Save/Restore of AtomSpace to ZeroMQ server" HAVE_ZMQ)
SUMMARY_ADD("Unit tests" "Unit tests" CXXTEST_FOUND)
SUMMARY_SHOW()
