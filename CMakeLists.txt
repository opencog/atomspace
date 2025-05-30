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
# cogutils already requires 3.12, so may as well ask for that.
# Correct handling of guile module install requires cmake version 3.12
# or newer, but Ubuntu Bionic 18.04 only has 3.10. Bummer about that.
CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

# Python venv seems to need this.
IF(CMAKE_VERSION VERSION_GREATER 3.31)
   CMAKE_POLICY(SET CMP0177 NEW)
ENDIF(CMAKE_VERSION VERSION_GREATER 3.31)

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

# Default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
                -DPROJECT_BINARY_DIR="${CMAKE_BINARY_DIR}")


# Version string is the git hash
EXECUTE_PROCESS(
	COMMAND git rev-parse --short HEAD
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	OUTPUT_VARIABLE GIT_COMMIT_HASH
	OUTPUT_STRIP_TRAILING_WHITESPACE)

ADD_DEFINITIONS(-DGIT_COMMIT_HASH="${GIT_COMMIT_HASH}")

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
# Find Guile. Required.
include(OpenCogFindGuile)

# ----------------------------------------------------------
# Find Python3 (optional; needed for Python3 bindings.)
include(OpenCogFindPython)

# ----------------------------------------------------------
# OCaml (optional; needed for the OCaml bindings.)

# At this time, OCaml does not build, because the cmake scripts fail to
# set up the include directory correctly. The work-around is to stub
# things out, below. Since the OCaml bindings are deprecated, anyway,
# this seems like an appropriate thing to do. A proper fix would be
# to tweak cogutils/cmake/FindOCaml.cmake so that it sets the include
# directory.
FIND_PACKAGE(OCaml)
IF (OCAML_FOUND)
	# ADD_DEFINITIONS(-DHAVE_OCAML)
	# SET(HAVE_OCAML 1)
	MESSAGE(STATUS "OCaml was found, but will not be built, due to bit-rot.")
ENDIF (OCAML_FOUND)

# ----------------------------------------------------------
# Glasgow Haskell compiler (Optional; needed for the Haskell bindings)
# The Haskell bindings are very old and unmaintained, and should not be
# used in development. Stub them out until a maintainer shows up and
# modernizes them.

FIND_PACKAGE(Stack)
IF (STACK_FOUND)
	# ADD_DEFINITIONS(-DHAVE_STACK)
	# SET(HAVE_STACK 1)
	MESSAGE(STATUS "Haskell was found, but will not be built, due to bit-rot.")
ENDIF (STACK_FOUND)

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
FILE(READ "${CMAKE_SOURCE_DIR}/opencog/atomspace/version.h" _ATOMSPACE_H_CONTENTS)
STRING(REGEX MATCH
	"#define ATOMSPACE_VERSION_STRING \"([0-9]+[.0-9]+[.0-9]+)\""
        _ "${_ATOMSPACE_H_CONTENTS}")
SET(SEMANTIC_VERSION ${CMAKE_MATCH_1})

# ===================================================================
# Include configuration.

# Set default include paths.
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR} ${CMAKE_BINARY_DIR}
	${COGUTIL_INCLUDE_DIR})

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
	ADD_CUSTOM_TARGET(test_flow
		DEPENDS tests
		DEPENDS tests/atoms/flow
		WORKING_DIRECTORY tests/atoms/flow
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Flow/stream tests..."
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
# that it is owned by the builder thus avoiding 'Permission denied' error when
# packaging.
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
SUMMARY_ADD("Unit tests" "Unit tests" CXXTEST_FOUND)
SUMMARY_SHOW()
