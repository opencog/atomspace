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
# Correct handling of guile module install requires cmake version 3.12
# or newer, but ubuntu bionic 18.04 only has 3.10. Bummer about that.
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12.2)
IF (COMMAND CMAKE_POLICY)
	CMAKE_POLICY(SET CMP0003 NEW)
ENDIF (COMMAND CMAKE_POLICY)

IF(CMAKE_VERSION VERSION_GREATER 3.0.2)
	CMAKE_POLICY(SET CMP0037 OLD)
	CMAKE_POLICY(SET CMP0042 NEW)
ENDIF(CMAKE_VERSION VERSION_GREATER 3.0.2)
IF (CMAKE_VERSION VERSION_GREATER 3.9.0)
	CMAKE_POLICY(SET CMP0068 NEW)
ENDIF (CMAKE_VERSION VERSION_GREATER 3.9.0)

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
include(OpenCogLibOptions)
include(OpenCogInstallOptions)
include(Summary)

# SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=thread")

# ----------------------------------------------------------
# Check for boost. We need dynamic-linked, threaded libs by default.
SET(Boost_USE_STATIC_LIBS OFF)
SET(Boost_USE_MULTITHREADED ON)
SET(MIN_BOOST 1.46)

# Required boost packages
FIND_PACKAGE(Boost ${MIN_BOOST} REQUIRED COMPONENTS system)

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
# Optional, but needed for unit tests.

FIND_PACKAGE(Cxxtest)
IF (CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest found.")
ELSE (CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest missing: needed for unit tests.")
ENDIF (CXXTEST_FOUND)

# ----------------------------------------------------------
# Optional, uses slightly more efficient replacement for std::set

# Facebook Folly
# https://github.com/facebook/folly/blob/main/folly/container/F14.md
# promises a faster and more compact hash table. Just one problem:
# It does not make any difference in the "real world" benchmark
# I tried (the `bio-loop3.scm` benchmark from opencog/benchmark)
#
FIND_PACKAGE(Folly)
IF (FOLLY_FOUND)
	MESSAGE(STATUS "Folly found.")
	SET(HAVE_FOLLY 1)
	ADD_DEFINITIONS(-DHAVE_FOLLY)
ELSE (FOLLY_FOUND)
	MESSAGE(STATUS "Folly missing: provides more efficient std::set replacement.")
ENDIF (FOLLY_FOUND)

# ----------------------------------------------------------
# Find Guile
include(OpenCogFindGuile)

# ----------------------------------------------------------
# By default, this uses Python3.
# To use Python2 only, run the following in the build directory:
# rm CMakeCache.txt && cmake -DCMAKE_DISABLE_FIND_PACKAGE_Python3Interp=TRUE ..
include(OpenCogFindPython)

# ----------------------------------------------------------
# OCaml (optional; needed for the OCaml bindings.)

FIND_PACKAGE(OCaml)
IF (OCAML_FOUND)
	ADD_DEFINITIONS(-DHAVE_OCAML)
	SET(HAVE_OCAML 1)
ENDIF (OCAML_FOUND)

# ----------------------------------------------------------
# Glasgow Haskell compiler (Optional; needed for the Haskell bindings)

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
# Optional Postgres, needed for PostgresStorageNode

FIND_PACKAGE(PGSQL 9.5)
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
# Include configuration.

# Set default include paths.
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR} ${CMAKE_BINARY_DIR}
	${Boost_INCLUDE_DIRS} ${COGUTIL_INCLUDE_DIR})

# Macros that define how atom types get declared.
INCLUDE("${CMAKE_SOURCE_DIR}/cmake/OpenCogAtomTypes.cmake")

# Macros for building language-specific objects.
INCLUDE("${CMAKE_SOURCE_DIR}/cmake/OpenCogCython.cmake")
INCLUDE("${CMAKE_SOURCE_DIR}/cmake/OpenCogGuile.cmake")
INCLUDE("${CMAKE_SOURCE_DIR}/cmake/OpenCogOCaml.cmake")

# ==========================================================
# Decide what to build, based on the packages found.

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
		ADD_CUSTOM_TARGET(check
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
		ADD_CUSTOM_TARGET(check
			DEPENDS tests
			WORKING_DIRECTORY tests
			# COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process --output-on-failure $(ARGS)
			COMMENT "Running tests..."
		)

		# When CMP00037 is finally turned off, just remove these two
		# lines.
		ADD_CUSTOM_TARGET(test)
		ADD_DEPENDENCIES(test check)
	ENDIF (CMAKE_BUILD_TYPE STREQUAL "Coverage")

	ADD_CUSTOM_TARGET(test_atomese
		DEPENDS tests
		WORKING_DIRECTORY tests/atoms
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Atomese tests..."
	)
	ADD_CUSTOM_TARGET(test_atomspace
		DEPENDS tests
		WORKING_DIRECTORY tests/atomspace
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Atomspace tests..."
	)
	ADD_CUSTOM_TARGET(test_guile
		DEPENDS tests
		WORKING_DIRECTORY tests/scm
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Guile and Python-bridge tests..."
	)
	ADD_CUSTOM_TARGET(test_join
		DEPENDS tests
		DEPENDS tests/atoms/container
		WORKING_DIRECTORY tests/atoms/container
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Pattern Container tests..."
	)
	ADD_CUSTOM_TARGET(test_matrix
		DEPENDS tests
		WORKING_DIRECTORY tests/matrix
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Sparse Matrix tests..."
	)
	ADD_CUSTOM_TARGET(test_persist
		DEPENDS tests
		WORKING_DIRECTORY tests/persist/
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Persistance tests..."
	)
	ADD_CUSTOM_TARGET(test_python
		WORKING_DIRECTORY tests/cython
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Python and Cython tests..."
	)
	ADD_CUSTOM_TARGET(test_query
		DEPENDS tests
		WORKING_DIRECTORY tests/query
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Pattern Engine tests..."
	)
	ADD_CUSTOM_TARGET(test_sheaf
		DEPENDS tests
		WORKING_DIRECTORY tests/sheaf
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running parser tests..."
	)
ENDIF (CXXTEST_FOUND)

ADD_SUBDIRECTORY(examples EXCLUDE_FROM_ALL)

ADD_CUSTOM_TARGET (examples
	# Using CMAKE_BUILD_TOOL results in the cryptic error message:
	# warning: jobserver unavailable: using -j1.  Add `+' to parent make rule.
	# This is because make doesn't know how to pass jobserver args to
	# the submake.  So, instead, just use $(MAKE) (with round parens)
	# -- that will do the right thing.
	# COMMAND ${CMAKE_BUILD_TOOL}
	COMMAND $(MAKE)
	WORKING_DIRECTORY examples
	COMMENT "Building examples"
)

ADD_CUSTOM_TARGET(cscope
	COMMAND find opencog examples tests -name '*.cc' -o -name '*.h' -o -name '*.cxxtest' -o -name '*.scm' > ${CMAKE_SOURCE_DIR}/cscope.files
	COMMAND cscope -b
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	COMMENT "Generating CScope database"
)

# ===============================================================
# Packaging
# XXX FIXME. Packging below is stale/wrong/broken.
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
	"guile-3.0-dev (>= 3.0.1)"
	"python3-dev (>= 3.6.7)"
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

SUMMARY_ADD("Doxygen" "Code documentation" DOXYGEN_FOUND)
# SUMMARY_ADD("Folly" "Replacement for std::set" HAVE_FOLLY)
SUMMARY_ADD("Haskell bindings" "Haskell bindings" HAVE_STACK)
SUMMARY_ADD("OCaml bindings" "OCaML bindings" HAVE_OCAML)
SUMMARY_ADD("Python bindings" "Python (cython) bindings" HAVE_CYTHON)
SUMMARY_ADD("Python tests" "Python bindings nose tests" HAVE_NOSETESTS)
SUMMARY_ADD("Scheme bindings" "Scheme (guile) bindings" HAVE_GUILE)
SUMMARY_ADD("SQL ODBC bindings" "Save/Restore of AtomSpace to database via ODBC" ODBC_FOUND)
SUMMARY_ADD("Postgres StorageNode" "Save/Restore of AtomSpace to Postgres database" PGSQL_FOUND)
SUMMARY_ADD("Unit tests" "Unit tests" CXXTEST_FOUND)
SUMMARY_SHOW()
