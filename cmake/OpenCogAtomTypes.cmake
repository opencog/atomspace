#
# OpenCogAtomTypes.cmake
#
# Wrapper to build Atom Type bindings for various rpogramming languages.
# This code runs at compile time (when the user types 'make') and NOT
# a CMake time (when the user types 'cmake ..') This may be a source of
# confusion!  (It was for me.)
#

INCLUDE("${OC_CMAKE_PATH}/cmake/OpenCogGenTypes.cmake")
INCLUDE("${OC_CMAKE_PATH}/cmake/OpenCogGenCppTypes.cmake")
INCLUDE("${OC_CMAKE_PATH}/cmake/OpenCogGenPythonTypes.cmake")
INCLUDE("${OC_CMAKE_PATH}/cmake/OpenCogGenScmTypes.cmake")

IF (BUILD_CPP)
	OPENCOG_CPP_ATOMTYPES(
		${SCRIPT_FILE}
		${HEADER_FILE} ${DEFINITIONS_FILE} ${INHERITANCE_FILE})

ELSEIF (BUILD_SCM)
	OPENCOG_SCM_ATOMTYPES(${SCRIPT_FILE} ${SCM_FILE})

ELSEIF (BUILD_PYTHON)
	OPENCOG_PYTHON_ATOMTYPES(${SCRIPT_FILE} ${PYTHON_FILE})

ELSEIF (BUILD_CSP)
	# Three of them: C++, scheme and python
	OPENCOG_CPP_ATOMTYPES(
		${SCRIPT_FILE}
		${HEADER_FILE} ${DEFINITIONS_FILE} ${INHERITANCE_FILE})
	OPENCOG_SCM_ATOMTYPES(${SCRIPT_FILE} ${SCM_FILE})
	OPENCOG_PYTHON_ATOMTYPES(${SCRIPT_FILE} ${PYTHON_FILE})

ELSE ()
	MESSAGE(FATAL_ERROR "Don't know how to build that!")
ENDIF()
