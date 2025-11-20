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
#include <opencog/util/Logger.h>
#include <opencog/util/misc.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/executioncontext/Context.h>
#include <opencog/eval/EvaluatorPool.h>
#include "PythonEval.h"
#include "PyGILGuard.h"

#include <algorithm> // for std::count
#include <chrono>    // for std::chrono_literals
#include <utility>   // for std::forward

#ifdef __APPLE__
  #define secure_getenv getenv
#endif

// A simple callback wrapper to do RAII when coing out of scope,
// capable of handling exceptions. Boost provides this (in the form of
// BOOST_SCOPE_EXIT macros), but we want to avoid the overkill of boost.
// Some future C++ will provide this, but gcc does not, as of mid-2025.
// Anyway, this is so simple, it seems best to avoid the dependencies.
// Just do it ourselves.
template <typename Callback>
class scope_exit
{
	private:
		Callback _callback;
	public:
		explicit scope_exit(Callback&& callback)
			: _callback(std::forward<Callback>(callback)) {}

		~scope_exit() { _callback(); }
};

#define SCOPE_GUARD4(A,B,C,D) scope_exit scope_guard([&](void)
#define SCOPE_GUARD_END )

using namespace opencog;
using namespace std::chrono_literals;

/*
 * NOTE: The Python C API reference counting is very tricky and the
 * API inconsistently handles PyObject* object ownership.
 * DO NOT change the reference count calls below using Py_INCREF
 * and the corresponding Py_DECREF until you completely understand
 * the Python API concepts of "borrowed" and "stolen" references.
 * Make absolutely NO assumptions about the type of reference the API
 * will return. Instead, verify if it returns a "new" or "borrowed"
 * reference. When you pass PyObject* objects to the API, don't assume
 * you still need to decrement the reference count until you verify
 * that the exact API call you are making does not "steal" the
 * reference: SEE:
 *   https://docs.python.org/2/c-api/intro.html?highlight=steals#reference-count-details
 * Remember to look to verify the behavior of each and every Py_ API call.
 */

PythonEval::PythonEval(void) :
	_atomspace(nullptr)
{
	_eval_done = true;
	_paren_count = 0;
	global_python_initialize();
	initialize_python_objects_and_imports();
}

PythonEval::~PythonEval()
{
    GILGuard gil;
    Py_DECREF(_pyRootModule);
}

void PythonEval::set_atomspace(const AtomSpacePtr& asp)
{
	_atomspace = asp;
}

AtomSpacePtr PythonEval::get_atomspace(void)
{
	return _atomspace;
}

PythonEval* PythonEval::get_python_evaluator(const AtomSpacePtr& asp)
{
	return EvaluatorPool<PythonEval>::get_evaluator(asp);
}

PythonEval* PythonEval::get_python_evaluator(AtomSpace* as)
{
	return EvaluatorPool<PythonEval>::get_evaluator(as);
}

// ===========================================================
// Calling functions and applying functions to arguments
// Most of these are part of the public API.

/**
 * Get the user defined function.
 * On error throws an exception.
 */
PyObject* PythonEval::do_call_user_function(const std::string& moduleFunction,
                                            PyObject* pyArguments)
{
    // Get a reference to the user function.
    PyObject* pyUserFunc = get_function(moduleFunction);

    // Make sure the function is callable.
    if (!PyCallable_Check(pyUserFunc))
    {
        Py_DECREF(pyUserFunc);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' not callable!", moduleFunction.c_str());
    }

    // Execute the user function and store its return value.
    PyObject* pyReturnValue = PyObject_CallObject(pyUserFunc, pyArguments);

    Py_DECREF(pyUserFunc);
    Py_DECREF(pyArguments);

    // Check for errors and throw with proper exception type preserved
    if (PyErr_Occurred())
    {
        if (pyReturnValue) Py_DECREF(pyReturnValue);
        throw_python_exception(moduleFunction);
    }

    return pyReturnValue;
}


/**
 * Apply the user function to the arguments passed in varargs and
 * return the extracted Value.
 */
ValuePtr PythonEval::apply_v(AtomSpace* as,
                             const std::string& func,
                             Handle args)
{
    // Get the actual argument count, passed in the ListLink.
    if (args->get_type() != LIST_LINK)
        throw RuntimeException(TRACE_INFO,
            "Expecting arguments to be a ListLink!");

    ASGuard asg(as);
    GILGuard gil;

    // Get the python value object returned by this user function.
    ValuePtr vptr = call_user_function(func, args->getOutgoingSet());

    // Check if we got a valid ValuePtr
    if (not vptr)
    {
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return Atomese!",
            func.c_str());
    }

    return vptr;
}

/**
 * Call the user defined function with the provide atomspace argument.
 * This is a cut-n-paste of PythonEval::call_user_function but with
 * assorted hacks to handle the different argument type.
 *
 * On error throws an exception.
 */
void PythonEval::apply_as(const std::string& func,
                          AtomSpace* as_argument)
{
    // What if the user is working in one AtomSpace, but is manipulating
    // another? Then setting the context AtomSpace would be wrong.
    // I'm confused; I don't know what to do here.
    // ASGuard asg(as_argument);
    GILGuard gil;

    // Create the Python tuple for the function call with python
    // atomspace.
    PyObject* pyArguments = PyTuple_New(1);
    PyObject* pyAtomSpace = atomspace_py_object(AtomSpaceCast(as_argument));

    PyTuple_SetItem(pyArguments, 0, pyAtomSpace);

    // Execute the user function.
    PyObject* pyReturnValue = do_call_user_function(func, pyArguments);
    Py_DECREF(pyReturnValue);
}

// ===================================================================
// Private Execution helper functions

/**
 * Execute the python string at the __main__ module context.
 *
 * This replaces a call to PyRun_SimpleString which clears errors so
 * that a subsequent call to Py_Error() returns false. This version
 * does everything that PyRun_SimpleString does except it does not
 * call PyErr_Print() and PyErr_Clear().
 */
std::string PythonEval::execute_string(const char* command)
{
    // We use Py_file_input here, instead of Py_single_input, because
    // Py_single_input spews errors on blank lines.  However, the
    // flip-side is that simple expressions, such as 2+2, generate no
    // output at all when Py_file_input is used; they do generate output
    // when Py_single_input is used.
    //
    // In either case, the pyResult does not have any string
    // representation; using PyObject_Str(pyResult) and then
    // PyString_AsString to print it gives "None" in all situations.
    // Because of this, I don't know how to write a valid command
    // interpreter for the python shell ...
    PyObject* pyRootDictionary = PyModule_GetDict(_pyRootModule);
    PyObject* pyResult = PyRun_StringFlags(command,
            Py_file_input, pyRootDictionary, pyRootDictionary,
            nullptr);

    // Check for error before collecting the result.
    check_for_error();

    std::string retval;
    if (pyResult)
    {
        PyObject* obrep = PyObject_Repr(pyResult);
        PyObject* pyStr = nullptr;
        if (not PyBytes_Check(obrep))
        {
            pyStr = PyUnicode_AsEncodedString(obrep, "UTF-8", "strict");
            Py_DECREF(obrep);
            obrep = pyStr;
        }
        retval = PyBytes_AS_STRING(obrep);
        Py_DECREF(obrep);
        Py_DECREF(pyResult);
    }

    PyObject *f = PySys_GetObject((char *) "stdout");
    if (f) fsync(PyObject_AsFileDescriptor(f));  // Force a flush

    return retval;
}


/// Exactly the same as execute_string, but the GIL lock is taken.
std::string PythonEval::execute_script(const std::string& script)
{
    OC_ASSERT(nullptr != _atomspace, "PythonEval AtomSpace not set!");
    ASGuard asg(_atomspace);

    GILGuard gil;
    check_for_error();
    std::string rc = execute_string(script.c_str());
    check_for_error();
    return rc;
}

/// Exactly the same as execute_script, but stdout is redirected.
/// Grab what is printed on stdout, and save it, so that we can
/// then return it to the user.
//
std::string PythonEval::exec_wrap_stdout(const std::string& expr)
{
    // Capture whatever python prints to stdout
    // What used to be stdout will now go to the pipe.
    int pipefd[2];
    int rc = pipe(pipefd);
    OC_ASSERT(0 == rc, "pipe creation failure");
    int stdout_backup = dup(fileno(stdout));
    OC_ASSERT(0 < stdout_backup, "stdout dup failure");
    rc = dup2(pipefd[1], fileno(stdout));
    OC_ASSERT(0 < rc, "pipe splice failure");

    SCOPE_GUARD4(&pipefd, &rc, &stdout_backup, &_capture_stdout)
    {
        // Restore stdout
        fflush(stdout);
        rc = write(pipefd[1], "", 1); // null-terminated string!
        OC_ASSERT(0 < rc, "pipe termination failure");
        rc = close(pipefd[1]);
        OC_ASSERT(0 == rc, "pipe close failure");
        rc = dup2(stdout_backup, fileno(stdout)); // restore stdout
        OC_ASSERT(0 < rc, "restore stdout failure");

        char buf[4097];
        int nr = read(pipefd[0], buf, sizeof(buf)-1);
        while (0 < nr)
        {
           buf[nr] = 0;
           if (1 < nr or 0 != buf[0]) _capture_stdout += buf;

           nr = read(pipefd[0], buf, sizeof(buf)-1);
        }

        // Cleanup.
        close(pipefd[0]);
    } SCOPE_GUARD_END;

    std::string res;
    try {
        res = execute_script(expr);
    } catch (const SilentException&) {
        res = _error_string;
    }
    return res;
}

void PythonEval::eval_expr_line(const std::string& partial_expr)
{
    // Trim whitespace, and comments before doing anything,
    // Otherwise, the various checks below fail.
    std::string part = partial_expr.substr(0,
                     partial_expr.find_last_not_of(" \t\n\r") + 1);

    size_t cmnt = part.find('#');
    if (std::string::npos != cmnt)
        part = part.substr(0, cmnt);

    // If we get a newline by itself, just ignore it.
    // Ignore leading comments; don't ignore empty line.
    int c = 0;
    size_t part_size = part.size();
    if (0 == part_size and 0 < partial_expr.size()) goto wait_for_more;

    if (0 < part_size) c = part[0];

    logger().debug("[PythonEval] get line:\n%s\n", partial_expr.c_str());
    // Check if there are open parentheses. If so, then we must
    // assume there will be more input that closes them off.
    {
        size_t open = std::count(part.begin(), part.end(), '(');
        size_t close = std::count(part.begin(), part.end(), ')');
        _paren_count += open - close;
        if (0 < _paren_count) goto wait_for_more;
    }

    // If the line starts with whitespace (tab or space) then assume
    // that it is standard indentation, and wait for the first
    // unindented line (or end-of-file).
    if (' ' == c or '\t' == c) goto wait_for_more;

    // If the line ends with a colon, its not a complete expression,
    // and we must wait for more input, i.e. more input is pending.
    if (0 < part_size and part.find_last_of(":\\") + 1 == part_size)
        goto wait_for_more;

    _input_line += part;
    _input_line += '\n';  // we stripped this off, above
    logger().debug("[PythonEval] eval_expr length=%zu:\n%s",
                  _input_line.length(), _input_line.c_str());

    _result = exec_wrap_stdout(_input_line);

    _input_line = "";
    _paren_count = 0;
    _pending_input = false;
    logger().debug("[PythonEval] eval_expr result length=%zu:\n%s",
                  _result.length(), _result.c_str());

    _eval_done = true;
    _wait_done.notify_all();
    return;

wait_for_more:
    _caught_error = false;
    _error_string = "";
    _pending_input = true;
    // Add this expression to our evaluation buffer.
    _input_line += part;
    _input_line += '\n';  // we stripped this off, above

    _result = "";
    _eval_done = true;
    _wait_done.notify_all();
}

// ===================================================================
// Public Execution API

void PythonEval::begin_eval()
{
    _eval_done = false;
    _caught_error = false;
    _pending_input = false;
    _result = "";
    _capture_stdout = "";
}

void PythonEval::eval_expr(const std::string& partial_expr)
{
    // XXX FIXME this does a lot of wasteful string copying.
    std::string expr = partial_expr;
    size_t nl = expr.find_first_of("\n\r");
    while (std::string::npos != nl)
    {
        if ('\r' == expr[nl]) nl++;
        if ('\n' == expr[nl]) nl++;
        std::string part = expr.substr(0, nl);
        eval_expr_line(part);
        expr = expr.substr(nl);
        nl = expr.find_first_of("\n\r");
    }
    eval_expr_line(expr);
}

std::string PythonEval::poll_result()
{
    // We don't have a real need to lock anything here; we're just
    // using this as a hack, so that the condition variable will
    // wake us up. The goal here is to block when there's no output
    // to be reported. We're doing this in a spinloop, because sometimes
    // ... I dunno, the cogserver hangs ... here ... !? Something's
    // buggy.
    std::unique_lock<std::mutex> lck(_poll_mtx);
    while (not _eval_done)
    {
        _wait_done.wait_for(lck, 100ms);
    }
    lck.unlock();

    if (0 == _result.compare("None")) _result.clear();
    std::string r = _capture_stdout + _result;

    // Add the missing newline
    if (0 < _result.size()) r += "\n";

    // Report the error string too, but only the first time.
    // Except this is already reported, when the exception is caught ...
    // if (_caught_error and 0 < _result.size()) r += _error_string + "\n";

    _result.clear();
    _capture_stdout.clear();
    return r;
}

void PythonEval::interrupt(void)
{
    // What we want to do here is to somehow interrupt or throw an
    // exception to the code that is running in the PyRun(), up above,
    // in the execute_string() method. That is, we want to make it
    // stop whatever infinite loop the user told it to run, and just
    // return to the C code (possibly spewing exceptions, etc.)
    // However, I cannot figure out how to implement this ...
    _result += "PythonEval: interrupt not implemented!\n";

    logger().warn("[PythonEval] interrupt not implemented!\n");
}

extern "C" {
// Thin wrapper for easy dlopen/dlsym dynamic loading
opencog::PythonEval* get_python_evaluator(opencog::AtomSpace* as)
{
	return opencog::PythonEval::get_python_evaluator(as);
}

};

// =========== END OF FILE =========
