/*
 * @file opencog/cython/PythonError.cc
 * @author Zhenhua Cai <czhedu@gmail.com>
 *         Ramin Barati <rekino@gmail.com>
 *         Keyvan Mir Mohammad Sadeghi <keyvan@opencog.org>
 *         Curtis Faith <curtis.m.faith@gmail.com>
 *         Alexey Potapov <alexey@singularitynet.io>
 * @date 2011-09-20
 *
 * Python error handling and message formatting.
 * Provides detailed traceback information for remote debugging.
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

// Python 3.7-3.8 compatibility: these macros were added in Python 3.9
#if PY_VERSION_HEX < 0x03090000
    #define PyFrame_GetCode(frame) ((frame)->f_code)
    #define PyFrame_GetLineNumber(frame) ((frame)->f_lineno)
#endif

// ===================================================================
// Error handling - Helper functions

/**
 * Helper: Try to get source code line from file using linecache module.
 * Returns empty string if unavailable.
 * Defensive - will not throw exceptions.
 */
static std::string try_get_source_line(const char* filename, int line_number)
{
    if (!filename || line_number <= 0) {
        return "";
    }

    // Use Python's linecache module (what Python uses internally)
    PyObject* linecache = PyImport_ImportModule("linecache");
    if (!linecache) {
        PyErr_Clear();
        return "";
    }

    PyObject* getline = PyObject_GetAttrString(linecache, "getline");
    Py_DECREF(linecache);
    if (!getline) {
        PyErr_Clear();
        return "";
    }

    // Call linecache.getline(filename, line_number)
    PyObject* args = Py_BuildValue("(si)", filename, line_number);
    if (!args) {
        Py_DECREF(getline);
        PyErr_Clear();
        return "";
    }

    PyObject* line = PyObject_CallObject(getline, args);
    Py_DECREF(getline);
    Py_DECREF(args);

    if (!line) {
        PyErr_Clear();
        return "";
    }

    const char* line_str = PyUnicode_AsUTF8(line);
    std::string result;
    if (line_str) {
        result = line_str;
        // Strip trailing newline
        if (!result.empty() && result.back() == '\n') {
            result.pop_back();
        }
        // Strip trailing carriage return (Windows)
        if (!result.empty() && result.back() == '\r') {
            result.pop_back();
        }
    }

    Py_DECREF(line);
    return result;
}

/**
 * Tier 1: Try to format exception using Python's traceback.format_exception().
 * This provides the richest output including source code lines.
 * Returns empty string on failure (will fall back to Tier 2).
 * Defensive - will not throw exceptions or leave error state.
 */
static std::string try_format_with_traceback_module(
    PyObject* pyErrorType,
    PyObject* pyError,
    PyObject* pyTraceback,
    const std::string& function_name)
{
    // Import traceback module
    PyObject* traceback_module = PyImport_ImportModule("traceback");
    if (!traceback_module) {
        PyErr_Clear();
        return "";
    }

    // Get format_exception function
    PyObject* format_exception = PyObject_GetAttrString(
        traceback_module, "format_exception"
    );
    Py_DECREF(traceback_module);
    if (!format_exception) {
        PyErr_Clear();
        return "";
    }

    // Build arguments: (type, value, traceback)
    // Need to increment refs because PyTuple_SetItem steals references
    Py_INCREF(pyErrorType);
    Py_INCREF(pyError);
    if (pyTraceback) {
        Py_INCREF(pyTraceback);
    }

    PyObject* args = PyTuple_New(3);
    PyTuple_SetItem(args, 0, pyErrorType);
    PyTuple_SetItem(args, 1, pyError);
    PyTuple_SetItem(args, 2, pyTraceback ? pyTraceback : Py_None);
    if (!pyTraceback) {
        Py_INCREF(Py_None);
    }

    // Call format_exception()
    PyObject* formatted_list = PyObject_CallObject(format_exception, args);
    Py_DECREF(format_exception);
    Py_DECREF(args);

    if (!formatted_list) {
        PyErr_Clear();
        return "";
    }

    // format_exception returns a list of strings
    std::stringstream result;

    // Add context header if function name provided
    if (function_name != NO_FUNCTION_NAME) {
        result << "Python error in " << function_name << ":\n\n";
    } else {
        result << "Python error:\n\n";
    }

    // Concatenate all strings from the list
    Py_ssize_t size = PyList_Size(formatted_list);
    for (Py_ssize_t i = 0; i < size; i++) {
        PyObject* line = PyList_GetItem(formatted_list, i);  // Borrowed ref
        if (line && PyUnicode_Check(line)) {
            const char* str = PyUnicode_AsUTF8(line);
            if (str) {
                result << str;
            }
        }
    }

    Py_DECREF(formatted_list);
    return result.str();
}

/**
 * Tier 2: Manual formatting with enhanced information.
 * Shows exception type, message, traceback with source lines,
 * and exception chaining (__cause__ and __context__).
 */
static std::string format_exception_manually(
    PyObject* pyErrorType,
    PyObject* pyError,
    PyObject* pyTraceback,
    const std::string& function_name)
{
    std::stringstream result;

    // === Header ===
    if (function_name != NO_FUNCTION_NAME) {
        result << "Python error in " << function_name;
    } else {
        result << "Python error";
    }

    // === Get exception type name ===
    const char* exception_type_name = nullptr;
    if (pyErrorType && PyType_Check(pyErrorType)) {
        PyObject* type_name_obj = PyObject_GetAttrString(pyErrorType, "__name__");
        if (type_name_obj) {
            exception_type_name = PyUnicode_AsUTF8(type_name_obj);
            Py_DECREF(type_name_obj);
        }
    }

    // === Get error message ===
    const char* error_message = nullptr;
    PyObject* pyErrorString = nullptr;
    if (pyError) {
        pyErrorString = PyObject_Str(pyError);
        if (pyErrorString) {
            error_message = PyUnicode_AsUTF8(pyErrorString);
        }
    }

    // Format: (ExceptionType: message) or just message
    if (exception_type_name) {
        result << " (" << exception_type_name;
        if (error_message) {
            result << ": " << error_message;
        }
        result << ")";
    } else if (error_message) {
        result << ": " << error_message;
    }
    result << "\n\n";

    if (pyErrorString) {
        Py_DECREF(pyErrorString);
    }

    // === Traceback ===
    if (pyTraceback) {
        result << "Traceback (most recent call last):\n";

        PyTracebackObject* tb = (PyTracebackObject*)pyTraceback;

        while (tb != nullptr) {
            // Defensive checks for NULL pointers
            if (!tb->tb_frame) {
                tb = tb->tb_next;
                continue;
            }

            PyFrameObject* frame = tb->tb_frame;
            PyCodeObject* code = PyFrame_GetCode(frame);

            if (!code) {
                tb = tb->tb_next;
                continue;
            }

            int line_number = tb->tb_lineno;

            // Get filename and function name
            const char* filename = nullptr;
            const char* code_name = nullptr;

            if (code->co_filename) {
                filename = PyUnicode_AsUTF8(code->co_filename);
            }
            if (code->co_name) {
                code_name = PyUnicode_AsUTF8(code->co_name);
            }

            // Format traceback line
            result << "  File \""
                   << (filename ? filename : "<unknown>")
                   << "\", line "
                   << line_number
                   << ", in "
                   << (code_name ? code_name : "<unknown>")
                   << "\n";

            // Try to show source line
            if (filename) {
                std::string source_line = try_get_source_line(filename, line_number);
                if (!source_line.empty()) {
                    result << "    " << source_line << "\n";
                }
            }

            tb = tb->tb_next;
        }
    }

    // === Exception chaining (Python 3.3+) ===
    // Show __cause__ (explicit "raise X from Y")
    PyObject* cause = PyException_GetCause(pyError);
    if (cause && cause != Py_None) {
        result << "\nThe above exception was the direct cause of the following exception:\n\n";
        PyObject* cause_str = PyObject_Str(cause);
        if (cause_str) {
            const char* cause_msg = PyUnicode_AsUTF8(cause_str);
            if (cause_msg) {
                result << "  " << cause_msg << "\n";
            }
            Py_DECREF(cause_str);
        }
        Py_DECREF(cause);
    }

    // Show __context__ (implicit exception during handling)
    if (!cause || cause == Py_None) {
        PyObject* context = PyException_GetContext(pyError);
        if (context && context != Py_None) {
            result << "\nDuring handling of the above exception, another exception occurred:\n\n";
            PyObject* context_str = PyObject_Str(context);
            if (context_str) {
                const char* context_msg = PyUnicode_AsUTF8(context_str);
                if (context_msg) {
                    result << "  " << context_msg << "\n";
                }
                Py_DECREF(context_str);
            }
            Py_DECREF(context);
        }
    }

    return result.str();
}

/**
 * Build the Python error message for the current error.
 * Uses a three-tier approach for maximum information with crash protection:
 * 1. Try traceback.format_exception() for richest output
 * 2. Fall back to enhanced manual formatting
 * 3. Ultimate fallback: minimal error message
 *
 * This error is delivered as a string to the user of the python
 * evaluator. This user will typically be accessing the evaluator
 * remotely, via the cogserver, and will not have any debugging
 * console, nor even a "normal" python REPL shell. Thus, we try
 * to provide as much debug info as reasonable, given they have
 * no other tools available.
 *
 * FWIW, the primary goal here is to make sure this code never crashes,
 * as it would bring down the cogserver. So that's rule one: don't
 * crash, even if the python error is insane.
 */
std::string PythonEval::build_python_error_message(
                                     const std::string& function_name)
{
    // Fetch the error from Python (clears the error indicator)
    PyObject *pyErrorType, *pyError, *pyTraceback;
    PyErr_Fetch(&pyErrorType, &pyError, &pyTraceback);

    // Normalize the exception (fills in missing traceback, etc.)
    PyErr_NormalizeException(&pyErrorType, &pyError, &pyTraceback);

    // Early exit if no error
    if (!pyError) {
        return "No error!";
    }

    // === Tier 1: Try using Python's traceback module ===
    std::string result = try_format_with_traceback_module(
        pyErrorType, pyError, pyTraceback, function_name
    );

    if (!result.empty()) {
        // Success! Clean up and return
        Py_XDECREF(pyErrorType);
        Py_XDECREF(pyError);
        Py_XDECREF(pyTraceback);
        return result;
    }

    // === Tier 2: Fall back to manual formatting ===
    result = format_exception_manually(
        pyErrorType, pyError, pyTraceback, function_name
    );

    // Clean up Python objects
    Py_XDECREF(pyErrorType);
    Py_XDECREF(pyError);
    Py_XDECREF(pyTraceback);

    // === Tier 3: Ultimate fallback ===
    if (result.empty()) {
        result = "Python error: <unable to format error details>";
    }

    return result;
}

// Throw a PythonException with the correct exception type preserved.
// This function extracts the Python exception type, formats an error message,
// and throws a PythonException without save/restore overhead.
void PythonEval::throw_python_exception(const std::string& function_name)
{
    // Fetch the error from Python (clears the error indicator)
    PyObject *pyErrorType, *pyError, *pyTraceback;
    PyErr_Fetch(&pyErrorType, &pyError, &pyTraceback);
    PyErr_NormalizeException(&pyErrorType, &pyError, &pyTraceback);

    // Extract Python exception type name
    std::string python_exc_type = "RuntimeError";  // default fallback
    if (pyErrorType && PyType_Check(pyErrorType)) {
        PyObject* type_name = PyObject_GetAttrString(pyErrorType, "__name__");
        if (type_name && PyUnicode_Check(type_name)) {
            python_exc_type = PyUnicode_AsUTF8(type_name);
            Py_DECREF(type_name);
        }
    }

    // Format error message using existing helpers
    std::string error_msg = try_format_with_traceback_module(
        pyErrorType, pyError, pyTraceback, function_name
    );

    if (error_msg.empty()) {
        error_msg = format_exception_manually(
            pyErrorType, pyError, pyTraceback, function_name
        );
    }

    if (error_msg.empty()) {
        error_msg = "Python error: <unable to format error details>";
    }

    // Clean up Python objects
    Py_XDECREF(pyErrorType);
    Py_XDECREF(pyError);
    Py_XDECREF(pyTraceback);
    PyErr_Clear();

    // Throw with original exception type preserved
    throw PythonException(python_exc_type, TRACE_INFO, "%s", error_msg.c_str());
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
