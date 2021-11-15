#
# OpenCogAtomTypes.cmake
#
# Old-style API: aDefinitions for automatically building the atom_types files, given
# a master file "atom_types.script" that defines all of the type
# relationships.
#
# Macro example call:
# XXX TBD

MESSAGE(STATUS "xduuude wtf ${OC_CMAKE_PATH}")

INCLUDE("${OC_CMAKE_PATH}/cmake/OpenCogGenTypes.cmake")

IF (BUILD_CSP)
	OPENCOG_GEN_ATOMTYPES(
		${SCRIPT_FILE}
		${HEADER_FILE} ${DEFINITIONS_FILE} ${INHERITANCE_FILE}
		${SCM_FILE}
		${PYTHON_FILE})
ELSE ()
	MESSAGE(FATAL_ERROR "Don't know how to build that!")
ENDIF()
