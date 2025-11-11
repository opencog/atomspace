/*
 * @file opencog/cython/PythonEval.cc
 * @author Zhenhua Cai <czhedu@gmail.com>
 *         Ramin Barati <rekino@gmail.com>
 *         Keyvan Mir Mohammad Sadeghi <keyvan@opencog.org>
 *         Curtis Faith <curtis.m.faith@gmail.com>
 *         Alexey Potapov <alexey@singularitynet.io>
 * @date 2011-09-20
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/util/exceptions.h>
#include "PythonEval.h"

using namespace opencog;

const char* NO_FUNCTION_NAME = "";

// ===================================================================
// Error handling

/**
 * Build the Python error message for the current error.
 */
std::string PythonEval::build_python_error_message(
                                     const std::string& function_name)
{
    // Get the error from Python.
    PyObject *pyErrorType, *pyError, *pyTraceback;
    PyErr_Fetch(&pyErrorType, &pyError, &pyTraceback);

    if (not pyError) return "No error!";

    // Construct the error message string.
    std::stringstream errorStringStream;
    errorStringStream << "Python error";

    if (function_name != NO_FUNCTION_NAME)
        errorStringStream << " in " << function_name;

    PyObject* pyErrorString = PyObject_Str(pyError);
    const char* pythonErrorString = PyUnicode_AsUTF8(pyErrorString);
    if (pythonErrorString) {
        errorStringStream << ": " << pythonErrorString << ".";
    } else {
        errorStringStream << ": Undescribed Error.";
    }

    // Print the traceback, too, if it is provided.
    if (pyTraceback)
    {
        errorStringStream << "\nTraceback (most recent call last):\n";

        PyTracebackObject* pyTracebackObject = (PyTracebackObject*)pyTraceback;

        while (pyTracebackObject != NULL)
        {
            int line_number = pyTracebackObject-> tb_lineno;

// Python 3.8 and earlier do not have these line-number macros
// Python 3.8 was released in October 2019
#if PY_VERSION_HEX < 0x03090000
    #define PyFrame_GetCode(frame) ((frame)->f_code)
    #define PyFrame_GetLineNumber(frame) ((frame)->f_lineno)
#endif
            const char* filename = PyUnicode_AsUTF8(
                PyFrame_GetCode(pyTracebackObject->tb_frame)->co_filename);
            const char* code_name = PyUnicode_AsUTF8(
                PyFrame_GetCode(pyTracebackObject->tb_frame)->co_name);

            errorStringStream << "File \"" << filename <<"\", ";
            errorStringStream << "line " << line_number <<", ";
            errorStringStream << "in " << code_name <<"\n";

            pyTracebackObject = pyTracebackObject -> tb_next;
        }
    }

    // Cleanup the references. NOTE: The traceback can be NULL even
    // when the others aren't.
    Py_DECREF(pyErrorType);
    Py_DECREF(pyError);
    Py_DECREF(pyErrorString);
    if (pyTraceback)
        Py_DECREF(pyTraceback);

    return errorStringStream.str();
}

// Check for errors in a script.
bool PythonEval::check_for_error()
{
    if (not PyErr_Occurred()) return false;

    std::string error_string = build_python_error_message(NO_FUNCTION_NAME);
    _input_line = "";
    PyErr_Clear();

    // Let the shell know what is going on.
    _caught_error = true;
    _error_string = error_string;
    _pending_input = false;
    _result = "";
    _eval_done = true;
    _wait_done.notify_all();
    throw SilentException();
}

// =========== END OF FILE =========
