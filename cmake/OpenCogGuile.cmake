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

# By default Guile return the path to its installation location.
# Such path will not work for users who wants to compile and install
# the project with a custom CMAKE_INSTALL_PREFIX. Compiling with
# custom PREFIX is a common practice. To support custom PREFIX
# this condition is added to override GUILE_SITE_DIR value using
# `cmake -DGUILE_SITE_DIR=...`.
IF (NOT DEFINED GUILE_SITE_DIR)
    IF(HAVE_GUILE)
        EXECUTE_PROCESS(COMMAND guile -c "(display (%site-dir))"
            OUTPUT_VARIABLE GUILE_SITE_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE)
    ENDIF()
ENDIF()
ADD_DEFINITIONS(-DGUILE_SITE_DIR="${GUILE_SITE_DIR}")

# Location of complied scheme files (*.go files) that the guile loader
# will search when looking for modules. Used in Makefiles to install
# the *.go files. Typical location is /usr/local/lib/guile/3.0/site-cache
# or similar. Over-ride by using
# `cmake -DGUILE_CCACHE_DIR=...`.
IF (NOT DEFINED GUILE_CCACHE_DIR)
    IF(HAVE_GUILE)
        EXECUTE_PROCESS(COMMAND guile -c "(display (cadr %load-compiled-path))"
            OUTPUT_VARIABLE GUILE_CCACHE_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE)
    ENDIF()
ENDIF()
# ADD_DEFINITIONS(-DGUILE_CCACHE_DIR="${GUILE_CCACHE_DIR}")

SET(GUILE_BIN_DIR "${CMAKE_BINARY_DIR}/opencog/scm")

# -------------------------------------------------------------------
#
# This configures the install and binary paths for each file.
# Variables set in the PARENTS_SCOPE context are MODULE_NAME and
# FILE_INSTALL_PATH. All other variables are local to this function.
FUNCTION(PROCESS_MODULE_STRUCTURE FILE_PATH)
    GET_PROPERTY(FILE_GENERATED SOURCE ${FILE_PATH}
        PROPERTY GENERATED SET)
    GET_FILENAME_COMPONENT(DIR_PATH ${FILE_PATH} DIRECTORY)
    GET_FILENAME_COMPONENT(FILE_NAME ${FILE_PATH} NAME)

    # Check if the file exists or is generated, and set
    # FULL_DIR_PATH or target dependencies.
    IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${DIR_PATH}/${FILE_NAME})
        SET(FULL_DIR_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${DIR_PATH}/")
    ELSEIF(EXISTS /${DIR_PATH}/${FILE_NAME})
        SET(FULL_DIR_PATH "/${DIR_PATH}/")
    ELSEIF(FILE_GENERATED AND (NOT SCM_DEPENDS))
        MESSAGE(FATAL_ERROR "The target that generates ${FILE_PATH} "
            "has not been added as a dependency using the keyword "
            "argument 'DEPENDS'")
    ELSEIF(FILE_GENERATED AND SCM_DEPENDS)
        ADD_DEPENDENCIES(${COPY_TARGET_NAME} ${SCM_DEPENDS})
        SET(FULL_DIR_PATH "/${DIR_PATH}/")
    ELSE()
        MESSAGE(FATAL_ERROR "${FILE_PATH} file does not exist in "
            "${CMAKE_CURRENT_SOURCE_DIR} nor does it have "
            "'GENERATED' property")
    ENDIF()

    # Specify module paths.
    STRING(REGEX MATCH
        "^(${PREFIX_DIR_PATH})([_a-z0-9/-]+)*/([_a-z0-9-]+)" ""
        ${SCM_MODULE_DESTINATION})

    # MODULE_NAME: it is equal to the MODULE_DESTINATION
    #              directory name
    # MODULE_FILE_DIR_PATH: the directory path where the
    #              MODULE_FILE is installed.
    # MODULE_DIR_PATH: the directory path where the files
    #              associated with the module are installed
    #              at and copied to, with the exception
    #              of the MODULE_FILE.
    SET(MODULE_NAME ${CMAKE_MATCH_3})
    SET(MODULE_NAME ${CMAKE_MATCH_3} PARENT_SCOPE)
    SET(MODULE_FILE_DIR_PATH ${CMAKE_MATCH_2})
    SET(MODULE_DIR_PATH ${CMAKE_MATCH_2}/${CMAKE_MATCH_3})

    IF (SCM_MODULE)
        SET(MODULE_NAME ${SCM_MODULE})
        SET(MODULE_NAME ${SCM_MODULE} PARENT_SCOPE)
    ENDIF()
    IF (NOT MODULE_NAME)
        SET(MODULE_NAME "opencog")
        SET(MODULE_NAME "opencog" PARENT_SCOPE)
    ENDIF()
    IF (${MODULE_DIR_PATH} STREQUAL "/")
        SET(MODULE_DIR_PATH ${DIR_PATH})
    ENDIF()

    # Set the install path.
    IF ("${MODULE_NAME}.scm" STREQUAL "${FILE_NAME}")
        SET(FILE_BUILD_PATH "${GUILE_BIN_DIR}/${MODULE_FILE_DIR_PATH}")
        SET(FILE_INSTALL_PATH "${GUILE_SITE_DIR}/${MODULE_FILE_DIR_PATH}"
            PARENT_SCOPE
        )
    ELSE()
        SET(FILE_BUILD_PATH "${GUILE_BIN_DIR}/${MODULE_DIR_PATH}")
        SET(FILE_INSTALL_PATH "${GUILE_SITE_DIR}/${MODULE_DIR_PATH}"
            PARENT_SCOPE
        )
    ENDIF()

    # Copy files into the build directory, mirroring the install
    # path structure.
    EXECUTE_PROCESS(
        COMMAND ${CMAKE_COMMAND} -E make_directory ${FILE_BUILD_PATH})

    ADD_CUSTOM_COMMAND(TARGET ${COPY_TARGET_NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy "${FULL_DIR_PATH}/${FILE_NAME}"
             "${FILE_BUILD_PATH}/${FILE_NAME}")

ENDFUNCTION(PROCESS_MODULE_STRUCTURE)

# -------------------------------------------------------------------
#
# This compiles a guile module. First argument is the name of the
# module. Additional arguments are taken to be dependencies of the
# module (so that a recompile is forced, whenever a depedency changes).
FUNCTION(COMPILE_MODULE MODULE_NAME)
    SET(FILE_BUILD_PATH ${GUILE_BIN_DIR})

    GET_FILENAME_COMPONENT(DIR_PATH ${MODULE_NAME} DIRECTORY)
    SET(GO_INSTALL_PATH ${GUILE_CCACHE_DIR}/${DIR_PATH})

    # Strip out slashes in the module name.
    STRING(REPLACE "/" "_" PHONY_TARGET ${MODULE_NAME})

    # Create a phony target so that users can reference it directly.
    ADD_CUSTOM_TARGET(${PHONY_TARGET}_go ALL
        DEPENDS ${FILE_BUILD_PATH}/${MODULE_NAME}.go)

    # Remainder of the arguments is a list of dependencies.
    SET(MODULE_FILES ${ARGN})

    # Perform the actual compilation
    ADD_CUSTOM_COMMAND(
        OUTPUT ${FILE_BUILD_PATH}/${MODULE_NAME}.go
        COMMAND guild compile
                ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_NAME}.scm
                -o ${FILE_BUILD_PATH}/${MODULE_NAME}.go
                -L ${FILE_BUILD_PATH}
        DEPENDS ${MODULE_FILES}
        COMMENT "Compiling ${MODULE_NAME}.scm"
        VERBATIM)

    INSTALL(
        FILES ${FILE_BUILD_PATH}/${MODULE_NAME}.go
        DESTINATION ${GO_INSTALL_PATH})

ENDFUNCTION(COMPILE_MODULE)

# ---------------------------------------------------------------------
# When building, all files specifed are are copied to
# '${CMAKE_BINARY_DIR}/opencog/scm' following the same file tree
# as will be installed (to /usr/local/share/guile/site/3.0/opencog)
#
# It has four keyword arguments
#
# FILES: List of files to be installed/copied.
#
# MODULE_DESTINATION: The absolute path where the files associated
#   with the module are installed, with the exception of the
#   MODULE_FILE(see definition at top of this file). The path for
#   MODULE_FILE, is inferred from this argument, even if it is the
#   only file to be installed.
#
# DEPENDS: Optional argument. A list of any targets that must be built
#   before the module can be built. A typical use is to make sure that
#   the `atom_types.scm` file is created first, before building this
#   module.
#
# COMPILE: Optional keyword. If present, the module file (the first
#   file in the file-list) will be compiled into guile RTL bytecode,
#   and installed into the guile bytecode cache location.
#
FUNCTION(ADD_GUILE_MODULE)
  # Define the target that will be used to copy scheme files in the
  # current source directory to the build directory. This is done so
  # as to be able to run scheme unit-tests without having to run
  # 'make install'. The tilde ~ occurs in package management (Debian).
  STRING(REGEX REPLACE "[/~]" "_" _TARGET_NAME_SUFFIX ${CMAKE_CURRENT_SOURCE_DIR})
  SET(COPY_TARGET_NAME "COPY_TO_LOAD_PATH_IN_BUILD_DIR_FROM_${_TARGET_NAME_SUFFIX}")
  IF (NOT (TARGET ${COPY_TARGET_NAME}))
    ADD_CUSTOM_TARGET(${COPY_TARGET_NAME} ALL)
  ENDIF()

  IF(HAVE_GUILE)
    SET(PREFIX_DIR_PATH "${GUILE_SITE_DIR}")
    SET(options COMPILE)
    SET(oneValueArgs MODULE_DESTINATION MODULE)
    SET(multiValueArgs FILES DEPENDS)
    CMAKE_PARSE_ARGUMENTS(SCM "${options}" "${oneValueArgs}"
        "${multiValueArgs}" ${ARGN})

    # The SCM module is given by the name of the first file
    # in the list.
    LIST(GET SCM_FILES 0 SCM_MODULE_FILE)
    STRING(REPLACE ".scm" "" SCM_MODULE ${SCM_MODULE_FILE})

    IF(NOT DEFINED SCM_MODULE_DESTINATION)
        GET_FILENAME_COMPONENT(DIR_PATH ${SCM_MODULE} DIRECTORY)
        SET(SCM_MODULE_DESTINATION ${GUILE_SITE_DIR}/${DIR_PATH})
    ENDIF()

    # The keyword argument 'FILES' is required.
    IF(DEFINED SCM_FILES)

        # If the COMPILE keyword is set, then compile the module into
        # module.go RTL bytecode.
        IF (${SCM_COMPILE})
            COMPILE_MODULE(${SCM_MODULE} ${SCM_FILES})

            IF(${SCM_DEPENDS})
                # Strip out slashes in the module name.
                STRING(REPLACE "/" "_" PHONY_TARGET ${SCM_MODULE})

                ADD_DEPENDENCIES(${PHONY_TARGET}_go ${SCM_DEPENDS})
            ENDIF()
        ENDIF()

# Arghhh FILE TOUCH first appears in version 3.12.0
# Everyone else is screwed.  Well, that explains a lot.
if(${CMAKE_VERSION} VERSION_GREATER "3.11.0")

        # FILE_PATH is used for variable name because files in
        # sub-directories may be passed.
        FOREACH(FILE_PATH ${SCM_FILES})

            PROCESS_MODULE_STRUCTURE(${FILE_PATH})

            # If any file in the module is newer than the module
            # itself, then touch the module; this is needed to force
            # (reinstall and) recompilation of the module!
            INSTALL(CODE "
              IF(EXISTS
                     ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_NAME}.scm
                 AND NOT
                     ${CMAKE_CURRENT_SOURCE_DIR}/${FILE_PATH}
                   STREQUAL
                     ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_NAME}.scm
                 AND
                     ${CMAKE_CURRENT_SOURCE_DIR}/${FILE_PATH}
                   IS_NEWER_THAN
                     ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_NAME}.scm
                 )
                 MESSAGE(\"-- Touch: ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_NAME}.scm\")
                 MESSAGE(\"-- Newer: ${CMAKE_CURRENT_SOURCE_DIR}/${FILE_PATH}\")
                 FILE(TOUCH ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_NAME}.scm)
              ENDIF()
            ")

        ENDFOREACH()
endif()

        # Run the loop again, this time installing. We need to run
        # the loop a second time, because the file-touch above sets
        # time-stamps of files earlier in the loop.
        #
        # We install here, instead of in the PROCESS_MODULE_STRUCTURE
        # function, so as to avoid "Command INSTALL() is not
        # scriptable" errors. This error is hit when copying scheme
        # files auto-generated by the  OPENCOG_ADD_ATOM_TYPES macro.
        FOREACH(FILE_PATH ${SCM_FILES})

            PROCESS_MODULE_STRUCTURE(${FILE_PATH})

            INSTALL (FILES ${FILE_PATH}
                     DESTINATION ${FILE_INSTALL_PATH})
        ENDFOREACH()

    ELSE()
        IF(NOT DEFINED SCM_FILES)
            MESSAGE(FATAL_ERROR "The keyword argument 'SCM_FILES' is not set")
        ENDIF()

        IF(NOT DEFINED SCM_MODULE_DESTINATION)
            MESSAGE(FATAL_ERROR "The keyword argument "
                   "'SCM_MODULE_DESTINATION' is not set")
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

        ADD_TEST(NAME ${TEST_NAME}
            COMMAND guile -L ${PROJECT_BINARY_DIR}/opencog/scm
                      --use-srfi=64 ${FILE_PATH}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        IF (GUILE_LOAD_PATH)
            SET_PROPERTY(TEST ${TEST_NAME} PROPERTY
                ENVIRONMENT "GUILE_LOAD_PATH=${GUILE_LOAD_PATH}")
        ENDIF(GUILE_LOAD_PATH)
    ENDIF()
ENDFUNCTION(ADD_GUILE_TEST)

# ----------------------------------------------------------------------------
# Declare a new dummy target representing path configuration for Guile
# extensions.  This must be called, before calling ADD_GUILE_EXTENSION
# and WRITE_GUILE_CONFIG.
#
# CONFIG_TARGET: The new target name
#
# MODULE_NAME: A space separated string representing the full guile
# module, e.g. "opencog as-config". This is passed to (define-module ...)
# and is used for importing extension paths.
#
FUNCTION(DECLARE_GUILE_CONFIG_TARGET CONFIG_TARGET MODULE_NAME TEST_ENV_VAR)
    ADD_CUSTOM_TARGET(${CONFIG_TARGET} ALL)
    SET_TARGET_PROPERTIES(${CONFIG_TARGET}
        PROPERTIES MODULE_NAME "${MODULE_NAME}")
ENDFUNCTION(DECLARE_GUILE_CONFIG_TARGET)

# ----------------------------------------------------------------------------
# Link a compiled guile extension to the config target
#
# CONFIG_TARGET: The target previously declared with DECLARE_GUILE_CONFIG
#
# EXTENSION_TARGET: A shared library target that can be loaded as a
# guile extension.
#
# SYMBOL_NAME: The guile symbol to use to define the path to the
# extension library.  In the build directory, this will point to the
# exact path. When installed this will point to OpenCog's shared library
# install dir.
#
FUNCTION(ADD_GUILE_EXTENSION CONFIG_TARGET EXTENSION_TARGET SYMBOL_NAME)
    GET_TARGET_PROPERTY(EXT_LIB_PATH ${EXTENSION_TARGET} BINARY_DIR)
    GET_TARGET_PROPERTY(EXT_LIBS ${CONFIG_TARGET} EXT_LIBS)
    IF (EXT_LIBS)
        LIST(APPEND EXT_LIBS "${SYMBOL_NAME}|${EXT_LIB_PATH}")
    ELSE(EXT_LIBS)
        SET(EXT_LIBS "${SYMBOL_NAME}|${EXT_LIB_PATH}")
    ENDIF (EXT_LIBS)
    SET_TARGET_PROPERTIES(${CONFIG_TARGET} PROPERTIES EXT_LIBS "${EXT_LIBS}")
ENDFUNCTION(ADD_GUILE_EXTENSION)

# ----------------------------------------------------------------------------
# Write out a config module, based on all extensions linked to the config
# target.
#
# OUTPUT_FILE: Where to write the file
#
# CONFIG_TARGET: The target previously declared with DECLARE_GUILE_CONFIG
#
# SCM_IN_BUILD_DIR: Whether to generate a config file for the build dir
# or for installing to a system dir.
#
FUNCTION(WRITE_GUILE_CONFIG OUTPUT_FILE CONFIG_TARGET SCM_IN_BUILD_DIR)
    set(SCM_BUILD_PATHS "")
    set(SCM_INSTALL_PATHS "")
    get_target_property(SYMBOL_PATH_LIST ${CONFIG_TARGET} EXT_LIBS)
    get_target_property(MODULE_NAME ${CONFIG_TARGET} MODULE_NAME)

    FILE(WRITE "${OUTPUT_FILE}" "(define-module (${MODULE_NAME}))\n")
    foreach(PATH_PAIR ${SYMBOL_PATH_LIST})
        string(REPLACE "|" ";" SYMBOL_AND_PATH ${PATH_PAIR})
        list(GET SYMBOL_AND_PATH 0 SYMBOL)
        list(GET SYMBOL_AND_PATH 1 LIBPATH)

        IF (SCM_IN_BUILD_DIR)
            FILE(APPEND "${OUTPUT_FILE}"
                "(define-public ${SYMBOL} \"${LIBPATH}/\")\n")
        ELSE (SCM_IN_BUILD_DIR)
            FILE(APPEND "${OUTPUT_FILE}"
                "(define-public ${SYMBOL} \"${CMAKE_INSTALL_PREFIX}/lib${LIB_DIR_SUFFIX}/opencog/\")\n")

        ENDIF (SCM_IN_BUILD_DIR)
    endforeach()

ENDFUNCTION(WRITE_GUILE_CONFIG)
