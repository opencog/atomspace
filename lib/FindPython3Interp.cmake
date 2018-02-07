#.rst:
# FindPython3Interp
# ----------------
#
# Find python 3 interpreter (based on original FindPythonInterp from cmake)
#
# This module finds if Python interpreter is installed and determines
# where the executables are.  This code sets the following variables:
#
# ::
#
#   PYTHON3INTERP_FOUND         - Was the Python executable found
#   PYTHON3_EXECUTABLE          - path to the Python interpreter
#
#
#
# ::
#
#   PYTHON3_VERSION_STRING      - Python version found e.g. 3.5.1
#   PYTHON3_VERSION_MAJOR       - Python major version found e.g. 3
#   PYTHON3_VERSION_MINOR       - Python minor version found e.g. 5
#   PYTHON3_VERSION_PATCH       - Python patch version found e.g. 1
#
#=============================================================================
# Copyright 2005-2010 Kitware, Inc.
# Copyright 2011 Bjoern Ricks <bjoern.ricks@gmail.com>
# Copyright 2012 Rolf Eike Beer <eike@sf-mail.de>
# Copyright 2016 Dominique Leuenberger <dimstar@opensuse.org>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

unset(_Python3_NAMES)

set(_Python3_VERSIONS 3.6 3.5 3.4 3.3 3.2 3.1 3.0)

if(Python3Interp_FIND_VERSION)
    if(Python3Interp_FIND_VERSION_COUNT GREATER 1)
        set(_PYTHON3_FIND_MAJ_MIN "${Python3Interp_FIND_VERSION_MAJOR}.${Python3Interp_FIND_VERSION_MINOR}")
        list(APPEND _Python3_NAMES
             python${_PYTHON3_FIND_MAJ_MIN}
             python${Python3Interp_FIND_VERSION_MAJOR})
        unset(_PYTHON3_FIND_OTHER_VERSIONS)
        if(NOT Python3Interp_FIND_VERSION_EXACT)
            foreach(_PYTHON3_V ${_PYTHON${Python3Interp_FIND_VERSION_MAJOR}_VERSIONS})
                if(NOT _PYTHON3_V VERSION_LESS _PYTHON3_FIND_MAJ_MIN)
                    list(APPEND _PYTHON3_FIND_OTHER_VERSIONS ${_PYTHON3_V})
                endif()
             endforeach()
        endif()
        unset(_PYTHON3_FIND_MAJ_MIN)
    else()
        list(APPEND _Python3_NAMES python${Python3Interp_FIND_VERSION_MAJOR})
        set(_PYTHON3_FIND_OTHER_VERSIONS ${_PYTHON${Python3Interp_FIND_VERSION_MAJOR}_VERSIONS})
    endif()
else()
    set(_PYTHON3_FIND_OTHER_VERSIONS ${_PYTHON3_VERSIONS})
endif()
find_program(PYTHON3_EXECUTABLE NAMES ${_Python3_NAMES})

# If FindPythonInterp has already found the major and minor version,
# insert that version next to get consistent versions of the interpreter and
# library.
if(DEFINED PYTHON3LIBS_VERSION_STRING)
  string(REPLACE "." ";" _PYTHON3LIBS_VERSION "${PYTHON3LIBS_VERSION_STRING}")
  list(GET _PYTHON3LIBS_VERSION 0 _PYTHON3LIBS_VERSION_MAJOR)
  list(GET _PYTHON3LIBS_VERSION 1 _PYTHON3LIBS_VERSION_MINOR)
  list(APPEND _Python3_VERSIONS ${_PYTHON3LIBS_VERSION_MAJOR}.${_PYTHON3LIBS_VERSION_MINOR})
endif()
# Search for the current active python version first
list(APPEND _Python3_VERSIONS ";")

unset(_PYTHON3_VERSIONS)

# Search for newest python version if python executable isn't found
if(NOT PYTHON3_EXECUTABLE)
    foreach(_CURRENT_VERSION IN LISTS _Python3_VERSIONS)
      set(_Python3_NAMES python${_CURRENT_VERSION})
      if(WIN32)
        list(APPEND _Python3_NAMES python3)
      endif()
      find_program(PYTHON3_EXECUTABLE
        NAMES ${_Python3_NAMES}
        PATHS [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]
        )
    endforeach()
endif()

# determine python version string
if(PYTHON3_EXECUTABLE)
    execute_process(COMMAND "${PYTHON3_EXECUTABLE}" -c
                            "import sys; sys.stdout.write(';'.join([str(x) for x in sys.version_info[:3]]))"
                    OUTPUT_VARIABLE _VERSION3
                    RESULT_VARIABLE _PYTHON3_VERSION_RESULT
                    ERROR_QUIET)
    if(NOT _PYTHON3_VERSION_RESULT)
        string(REPLACE ";" "." PYTHON3_VERSION_STRING "${_VERSION3}")
        list(GET _VERSION3 0 PYTHON3_VERSION_MAJOR)
        list(GET _VERSION3 1 PYTHON3_VERSION_MINOR)
        list(GET _VERSION3 2 PYTHON3_VERSION_PATCH)
        if(PYTHON3_VERSION_PATCH EQUAL 0)
            # it's called "Python 3.1", not "3.1.0"
            string(REGEX REPLACE "\\.0$" "" PYTHON3_VERSION_STRING "${PYTHON3_VERSION_STRING}")
        endif()
    else()
        # sys.version predates sys.version_info, so use that
        execute_process(COMMAND "${PYTHON3_EXECUTABLE}" -c "import sys; sys.stdout.write(sys.version)"
                        OUTPUT_VARIABLE _VERSION3
                        RESULT_VARIABLE _PYTHON3_VERSION_RESULT
                        ERROR_QUIET)
        if(NOT _PYTHON3_VERSION_RESULT)
            string(REGEX REPLACE " .*" "" PYTHON3_VERSION_STRING "${_VERSION3}")
            string(REGEX REPLACE "^([0-9]+)\\.[0-9]+.*" "\\1" PYTHON3_VERSION_MAJOR "${PYTHON3_VERSION_STRING}")
            string(REGEX REPLACE "^[0-9]+\\.([0-9])+.*" "\\1" PYTHON3_VERSION_MINOR "${PYTHON3_VERSION_STRING}")
            if(PYTHON3_VERSION_STRING MATCHES "^[0-9]+\\.[0-9]+\\.([0-9]+)")
                set(PYTHON3_VERSION_PATCH "${CMAKE_MATCH_1}")
            else()
                set(PYTHON3_VERSION_PATCH "0")
            endif()
        else()
            # sys.version was first documented for Python 1.5, so assume
            # this is older.
            set(PYTHON3_VERSION_STRING "1.4")
            set(PYTHON3_VERSION_MAJOR "1")
            set(PYTHON3_VERSION_MINOR "4")
            set(PYTHON3_VERSION_PATCH "0")
        endif()
    endif()
    unset(_PYTHON3_VERSION_RESULT)
    unset(_VERSION3)
endif()

# handle the QUIETLY and REQUIRED arguments and set PYTHON3INTERP_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Python3Interp REQUIRED_VARS PYTHON3_EXECUTABLE PYTHON3_VERSION_STRING)

mark_as_advanced(PYTHON3_EXECUTABLE)
