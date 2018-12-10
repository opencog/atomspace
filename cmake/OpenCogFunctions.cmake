# Copyright (C) 2016 OpenCog Foundation

# References:
# https://www.gnu.org/software/guile/manual/guile.html#Modules-and-the-File-System
# https://www.gnu.org/software/guile/manual/guile.html#Creating-Guile-Modules
# https://www.gnu.org/software/guile/manual/guile.html#Installing-Site-Packages

# Definitions:
#
# * MODULE_FILE: The name of the file that defines the module. It has
#   the same name as the directory it is in, or the name of the parent
#   directory of current directory if it is in a folder named 'scm'.
#   In addition this file shoule have a define-module expression
#   for it be importable, as per guile's specification. See reference
#   links above.

IF(HAVE_GUILE)
    EXECUTE_PROCESS(COMMAND guile -c "(display (%site-dir))"
        OUTPUT_VARIABLE GUILE_SITE_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE)
ENDIF()
ADD_DEFINITIONS(-DGUILE_SITE_DIR=\\"${GUILE_SITE_DIR}\\")

# ----------------------------------------------------------------------------
# This configures the install and binary paths for each file, passed to it,
# based on the value of the variables MODULE_NAME, MODULE_FILE_DIR_PATH and
# MODULE_DIR_PATH in the PARENT_SCOPE.
FUNCTION(PROCESS_MODULE_STRUCTURE FILE_NAME)
    SET(GUILE_BIN_DIR "${CMAKE_BINARY_DIR}/opencog/scm")

    # Copy files into build directory mirroring the install path structure.
    # Also configure for install.
    IF ("${MODULE_NAME}.scm" STREQUAL "${FILE_NAME}")
        EXECUTE_PROCESS(
            COMMAND ${CMAKE_COMMAND} -E make_directory ${GUILE_BIN_DIR}/${MODULE_FILE_DIR_PATH}
        )
        ADD_CUSTOM_COMMAND(
            OUTPUT "${GUILE_BIN_DIR}/${MODULE_FILE_DIR_PATH}/${FILE_NAME}"
            COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}" "${GUILE_BIN_DIR}/${MODULE_FILE_DIR_PATH}/${FILE_NAME}"
            DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}"
        )
        SET(MODULE_FILE_DEPEND "${GUILE_BIN_DIR}/${MODULE_FILE_DIR_PATH}/${FILE_NAME}"
            PARENT_SCOPE)
        SET(FILE_INSTALL_PATH "${GUILE_SITE_DIR}/${MODULE_FILE_DIR_PATH}"
            PARENT_SCOPE
        )
    ELSE()
        EXECUTE_PROCESS(
            COMMAND ${CMAKE_COMMAND} -E make_directory ${GUILE_BIN_DIR}/${MODULE_DIR_PATH}
        )
        ADD_CUSTOM_COMMAND(
            OUTPUT "${GUILE_BIN_DIR}/${MODULE_DIR_PATH}/${FILE_NAME}"
            COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}" "${GUILE_BIN_DIR}/${MODULE_DIR_PATH}/${FILE_NAME}"
            DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}"
        )
        SET(MODULE_FILE_DEPEND "${GUILE_BIN_DIR}/${MODULE_DIR_PATH}/${FILE_NAME}"
            PARENT_SCOPE)
        SET(FILE_INSTALL_PATH "${GUILE_SITE_DIR}/${MODULE_DIR_PATH}"
            PARENT_SCOPE
        )
    ENDIF()
ENDFUNCTION(PROCESS_MODULE_STRUCTURE)

# ----------------------------------------------------------------------------
# When building, all files specifed are are copied to
# '${CMAKE_BINARY_DIR}/opencog/scm' following the file tree structure created
# when installing to /usr/local/share/opencog/scm. It has two keyword arguments
#
# FILES: List of files to be installed/copied
#
# MODULE_DESTINATION: The absolute path where the files associated
#   with the module are installed, with the exception of the
#   MODULE_FILE(see definition at top of this file). The path for
#   MODULE_FILE, is inferred from this argument, even if it is the only file to
#   be installed.
FUNCTION(ADD_GUILE_MODULE)
  IF(HAVE_GUILE)
    SET(PREFIX_DIR_PATH "${GUILE_SITE_DIR}")
    SET(options "")  # This is used only as a place-holder
    SET(oneValueArgs MODULE_DESTINATION)
    SET(multiValueArgs FILES)
    CMAKE_PARSE_ARGUMENTS(SCM "${options}" "${oneValueArgs}"
        "${multiValueArgs}" ${ARGN})

    # NOTE:  The keyword arguments 'FILES' and 'MODULE_DESTINATION' are
    # required.
    IF((DEFINED SCM_FILES) AND (DEFINED SCM_MODULE_DESTINATION))
        SET(GUILE_MODULE_DEPENDS "")
        FOREACH(FILE_NAME ${SCM_FILES})
            # Check if the file exists in the current source directory.
            IF(NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME})
                MESSAGE(FATAL_ERROR "${FILE_NAME} file does not exist in "
                ${CMAKE_CURRENT_SOURCE_DIR})
            ENDIF()

            # Specify module paths.
            STRING(REGEX MATCH
                "^(${PREFIX_DIR_PATH})([a-z0-9/-]+)*/([a-z0-9-]+)" ""
                ${SCM_MODULE_DESTINATION})

            # MODULE_NAME: it is equal to the MODULE_DESTINATION directory name
            # MODULE_FILE_DIR_PATH: the directory path where the MODULE_FILE is
            #   installed.
            # MODULE_DIR_PATH: the directory path where the files associated
            #   with the module are installed at and copied to, with the exception
            #   of the MODULE_FILE.
            SET(MODULE_NAME ${CMAKE_MATCH_3})
            SET(MODULE_FILE_DIR_PATH ${CMAKE_MATCH_2})
            SET(MODULE_DIR_PATH ${CMAKE_MATCH_2}/${CMAKE_MATCH_3})

            PROCESS_MODULE_STRUCTURE(${FILE_NAME})
            # NOTE: The install configuration isn't part of
            # PROCESS_MODULE_STRUCTURE function so as to avoid "Command
            # INSTALL() is not scriptable" error, when using it in copying
            # scheme files during code-generation by the OPENCOG_ADD_ATOM_TYPES
            # macro.
            INSTALL (FILES
                ${FILE_NAME}
                DESTINATION ${FILE_INSTALL_PATH}
            )
            LIST(APPEND GUILE_MODULE_DEPENDS ${MODULE_FILE_DEPEND})
        ENDFOREACH()
        IF( NOT ( TARGET "${MODULE_NAME}_GUILE_INSTALL"))
            ADD_CUSTOM_TARGET("${MODULE_NAME}_GUILE_INSTALL" ALL DEPENDS "${GUILE_MODULE_DEPENDS}")
        ENDIF()
    ELSE()
        IF(NOT DEFINED SCM_FILES)
            MESSAGE(FATAL_ERROR "The keyword argument 'FILES' is not set in "
                ${CMAKE_CURRENT_LIST_FILE}:${CMAKE_CURRENT_LIST_LINE})
        ENDIF()

        IF(NOT DEFINED MODULE_DESTINATION)
            MESSAGE(FATAL_ERROR "The keyword argument 'MODULE_DESTINATION' "
            "is not set in "
            ${CMAKE_CURRENT_LIST_FILE}:${CMAKE_CURRENT_LIST_LINE})
        ENDIF()
    ENDIF()
  ENDIF()
ENDFUNCTION(ADD_GUILE_MODULE)

FUNCTION(ADD_GUILE_TEST TEST_NAME FILE_NAME)
    # srfi-64 is installed in guile 2.2 and above, thus check for it.
    IF(HAVE_GUILE AND (GUILE_VERSION VERSION_GREATER 2.2))
        SET(FILE_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}")
        # Check if the file exists in the current source directory.
        IF(NOT EXISTS ${FILE_PATH})
            MESSAGE(FATAL_ERROR "${FILE_NAME} file does not exist in "
                ${CMAKE_CURRENT_SOURCE_DIR})
        ENDIF()

        ADD_TEST(${TEST_NAME} guile --use-srfi=64
            -L ${DATADIR}/scm
            ${FILE_PATH}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

    ENDIF()
ENDFUNCTION(ADD_GUILE_TEST)
