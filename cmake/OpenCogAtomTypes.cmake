#
# OpenCogAtomTypes.cmake
#
# Old-style API: aDefinitions for automatically building the atom_types files, given
# a master file "atom_types.script" that defines all of the type
# relationships.
#
# Macro example call:
# XXX TBD

INCLUDE("${OC_CMAKE_PATH}/cmake/OpenCogGenTypes.cmake")

IF (BUILD_CPP)
	OPENCOG_CPP_ATOMTYPES(
		${SCRIPT_FILE}
		${HEADER_FILE} ${DEFINITIONS_FILE} ${INHERITANCE_FILE})

ELSEIF (BUILD_SCM)
	OPENCOG_SCM_ATOMTYPES(${SCRIPT_FILE} ${SCM_FILE})

ELSEIF (BUILD_PYTHON)
	OPENCOG_PYTHON_ATOMTYPES(${SCRIPT_FILE} ${PYTHON_FILE})

ELSE ()
	MESSAGE(FATAL_ERROR "Don't know how to build that!")
ENDIF()
