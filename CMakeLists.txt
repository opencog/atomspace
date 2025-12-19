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

# Cogutil
FIND_PACKAGE(CogUtil 2.2.1 CONFIG REQUIRED)
IF (NOT COGUTIL_FOUND)
	MESSAGE(FATAL_ERROR "CogUtil missing: it is needed!")
ENDIF ()

include(OpenCogGccOptions)
include(OpenCogLibOptions)
include(OpenCogInstallOptions)
include(Summary)

# SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=thread")

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
	# Disable. Hard to install, shows no improvments
	# ADD_DEFINITIONS(-DUSE_FOLLY)
ELSE (FOLLY_FOUND)
	MESSAGE(STATUS "Folly missing: provides more efficient std::set replacement.")
ENDIF (FOLLY_FOUND)

FIND_PACKAGE(SparseHash)
IF (SPARSEHASH_FOUND)
	MESSAGE(STATUS "SparseHash found.")
	SET(HAVE_SPARSEHASH 1)
	ADD_DEFINITIONS(-DHAVE_SPARSEHASH)

	# Set these directly in the header files. Setting them here also
	# works, but it's more complex and error prone. Harder to debug.
	# ADD_DEFINITIONS(-DUSE_SPARSE_INCOMING)
	# ADD_DEFINITIONS(-DUSE_SPARSE_TYPESET)
	# SET(COMPILE_TIME_DEFS
	#	"HAVE_SPARSEHASH;USE_SPARSE_INCOMING=1;USE_SPARSE_TYPESET=1")
	# Disable USE_SPARSE_KVP; see Atom.h for explanation.
	# ADD_DEFINITIONS(-DUSE_SPARSE_KVP)
	# The below is used in atomspace.pc for pkg-config
	# SET(ATOMSPACE_CFLAGS "-DHAVE_SPARSEHASH -DUSE_SPARSE_INCOMING")
ELSE (SPARSEHASH_FOUND)
	MESSAGE(STATUS "SparseHash missing: provides efficient std::set replacement.")
ENDIF (SPARSEHASH_FOUND)

# ----------------------------------------------------------
# Find Guile. Required.
include(OpenCogFindGuile)

# ----------------------------------------------------------
# Find Python3 (optional; needed for Python3 bindings.)
include(OpenCogFindPython)

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

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
	message(STATUS "Configuring for macOS...")
    
    # Add macOS-specific include and library paths
    include_directories(/opt/homebrew/include)
    link_directories(/opt/homebrew/lib)
endif()

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

# ----------------------------------------------------------
# Setup testing infrastructure (tests target, check target)
INCLUDE(OpenCogTestOptions)
OPENCOG_SETUP_TESTING()

# Project-specific test targets
OPENCOG_ADD_TEST_TARGET(test_atomese   tests/atoms      "Running Atomese tests...")
OPENCOG_ADD_TEST_TARGET(test_atomspace tests/atomspace  "Running Atomspace tests...")
OPENCOG_ADD_TEST_TARGET(test_flow      tests/atoms/flow "Running Flow/stream tests...")
OPENCOG_ADD_TEST_TARGET(test_guile     tests/scm        "Running Guile tests...")
OPENCOG_ADD_TEST_TARGET(test_join      tests/atoms/container "Running Container tests...")
OPENCOG_ADD_TEST_TARGET(test_python    tests/cython     "Running Python tests...")
OPENCOG_ADD_TEST_TARGET(test_query     tests/query      "Running Pattern Engine tests...")

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
	"libcogutil-dev (>= 2.2.1)"
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
# Don't print, it's never used.
# SUMMARY_ADD("Folly" "Replacement for std::set" HAVE_FOLLY)
SUMMARY_ADD("Python bindings" "Python (cython) bindings" HAVE_CYTHON)
SUMMARY_ADD("Python tests" "Python bindings pytest tests" HAVE_PYTEST)
SUMMARY_ADD("Scheme bindings" "Scheme (guile) bindings" HAVE_GUILE)
SUMMARY_ADD("SparseHash" "Replacement for std::set" HAVE_SPARSEHASH)
SUMMARY_ADD("Unit tests" "Unit tests" CXXTEST_FOUND)
SUMMARY_SHOW()
