/*
 * @file opencog/cython/PythonLoader.cc
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

#include <vector>

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/executioncontext/Context.h>
#include "PythonEval.h"
#include "PyGILGuard.h"

// This is a header in the build directory, auto-gened by cython.
// It can only ever be included just once, over all c++ files.
// More than one inclusion leads to obscure runtime errors.
#include "opencog/atomspace_api.h"

#include <dlfcn.h>

using namespace opencog;

const int NO_SIGNAL_HANDLERS = 0;

// ============================================================
// Python system initialization

static bool already_initialized = false;
static bool initialized_outside_opencog = false;
static void *_dlso = nullptr;

void opencog::global_python_initialize()
{
    // Don't initialize twice
    if (already_initialized) return;
    already_initialized = true;

    logger().info("[global_python_initialize] Start");

    _dlso = dlopen(PYLIBNAME, RTLD_LAZY | RTLD_GLOBAL);

    // We don't really know the gstate yet but we'll set
    // it here to avoid compiler warnings below.
    PyGILState_STATE gstate = PyGILState_UNLOCKED;

    // Start up Python.
    if (Py_IsInitialized())
    {
        // If we were already initialized then someone else did it.
        initialized_outside_opencog = true;

        // Just grab the GIL
        gstate = PyGILState_Ensure();
    }
    else
    {
        // We are doing the initialization.
        initialized_outside_opencog = false;

        // Initialize Python (InitThreads grabs GIL implicitly)
        Py_InitializeEx(NO_SIGNAL_HANDLERS);

// Python 3.9 came out in October 2020. Remove this in 2026/2027
#if PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION < 9
        PyEval_InitThreads();
#endif
    }

    // Initialize the auto-generated Cython api.
    import_opencog__atomspace();

    // The import_opencog__atomspace() call above sets the
    // py_atomspace() function pointer if the cython module load
    // succeeded. But the function pointer will be NULL if the
    // opencog.atomspace cython module failed to load. Avert
    // a hard-to-debug crash on null-pointer-deref, and replace
    // it by this hard-to-debug error message.
    if (nullptr == py_atomspace) {
        PyErr_Print();
        logger().warn("PythonEval::%s Failed to load the "
                       "opencog.atomspace module", __FUNCTION__);
    } else {
        // Create and set a default atomspace for the initial thread.
        // This allows type constructors to work without requiring
        // explicit set_default_atomspace() calls.
        AtomSpacePtr default_atomspace = createAtomSpace();
        push_context_atomspace(default_atomspace);
        logger().info("[global_python_initialize] Created default atomspace");
    }

    // Release the GIL, otherwise the Python shell hangs on startup.
    if (initialized_outside_opencog)
        PyGILState_Release(gstate);
    else
        // Several websites suggest that `PyEval_ReleaseLock()`
        // should be used here. However, this results in bug
        // opencog/atomspace#671. A closer reading of the official
        // python docs suggests that `PyEval_SaveThread` be used
        // instead, and indeed ... that works! Woo hoo!
        PyEval_SaveThread();

    logger().info("[global_python_initialize] Finish");
}

void opencog::global_python_finalize()
{
    logger().debug("[global_python_finalize] Start");
    if (!already_initialized)
        return;

    // Clear the context stack to release any atomspace references
    // before Python finalization. This prevents the thread_local
    // deque destructor from trying to access Python during shutdown.
    clear_context();

    // Cleanup Python.
    if (!initialized_outside_opencog)
    {
        PyGILState_Ensure(); // yes this is needed, see bug #671
        Py_Finalize();
        if (_dlso) dlclose(_dlso);
    }

    // No longer initialized.
    already_initialized = false;
    _dlso = nullptr;

    logger().debug("[global_python_finalize] Finish");
}

void PythonEval::initialize_python_objects_and_imports(void)
{
    // Grab the GIL
    GILGuard gil;

    // Get the __main__ module. NOTE: PyImport_AddModule returns
    // a borrowed reference so we must promote it with an increment.
    _pyRootModule = PyImport_AddModule("__main__");
    Py_INCREF(_pyRootModule);
    PyModule_AddStringConstant(_pyRootModule, "__file__", "");

    logger().debug("PythonEval::%s Finished initialising python evaluator.",
        __FUNCTION__);
}

PyObject* PythonEval::atomspace_py_object(AtomSpacePtr asp)
{
    // The py_atomspace function pointer will be NULL if the
    // opencog.atomspace cython module failed to load. Avert
    // a hard-to-debug crash on null-pointer-deref, and replace
    // it by a slightly less hard-to-debug error message.
    if (NULL == py_atomspace) {
        logger().warn("PythonEval::%s Failed to load the"
                      "opencog.atomspace module", __FUNCTION__);
        return NULL;
    }

    PyObject * pyAtomSpace = py_atomspace(asp);

    if (!pyAtomSpace) {
        if (PyErr_Occurred())
            PyErr_Print();

        logger().warn("PythonEval::%s Failed to get atomspace "
                      "wrapped with python object", __FUNCTION__);
    }

    return pyAtomSpace;
}

// ====================================================
// Finding python functions

/**
 * Get the Python function from a dotted name like "module.Class.method".
 * Returns a new reference that the caller must DECREF.
 */
PyObject* PythonEval::get_function(const std::string& moduleFunction)
{
    // Split into parts: "module.Class.method" -> ["module", "Class", "method"]
    std::vector<std::string> parts;
    size_t start = 0;
    size_t dot = moduleFunction.find('.');

    while (dot != std::string::npos)
    {
        parts.push_back(moduleFunction.substr(start, dot - start));
        start = dot + 1;
        dot = moduleFunction.find('.', start);
    }
    parts.push_back(moduleFunction.substr(start));

    // Start with __main__ or import the first part as a module
    PyObject* obj = nullptr;
    size_t i = 0;

    if (parts.size() > 1)
    {
        // Try to import first part as a module
        obj = PyImport_ImportModule(parts[0].c_str());
        if (obj) {
            i = 1;  // Successfully imported, continue from second part
        } else {
            // Not a module, clear error and try as __main__ attribute
            PyErr_Clear();
            obj = _pyRootModule;
            Py_INCREF(obj);
            i = 0;
        }
    }
    else
    {
        // No dots, just a function name in __main__
        obj = _pyRootModule;
        Py_INCREF(obj);
    }

    // Walk the remaining dotted path
    for (; i < parts.size(); i++)
    {
        PyObject* next = PyObject_GetAttrString(obj, parts[i].c_str());
        Py_DECREF(obj);

        if (!next)
        {
            PyErr_Clear();
            throw RuntimeException(TRACE_INFO,
                "Python object '%s' not found in '%s'!",
                parts[i].c_str(), moduleFunction.c_str());
        }

        obj = next;
    }

    return obj;  // Returns new reference
}

/**
 * Call the user defined function with the arguments passed in the
 * ListLink handle 'arguments'.
 *
 * On error throws an exception.
 */
ValuePtr PythonEval::call_user_function(const std::string& func,
                                        const HandleSeq& args)
{
    // Create the Python tuple for the function call with python
    // atoms for each of the atoms in the link arguments.
    size_t nargs = args.size();
    PyObject* pyArguments = PyTuple_New(nargs);
    for (size_t i=0; i<nargs; i++)
        PyTuple_SetItem(pyArguments, i, py_atom(args[i]));

    // Execute the user function and store its return value.
    PyObject* pyValue = do_call_user_function(func, pyArguments);

    // Get the C++ ValuePtr directly via the API.
    ValuePtr vptr = py_value_ptr(pyValue);

    Py_DECREF(pyValue);

    // The pyx wrapper code will return a null ValuePtr if it
    // stumbled on anything unexpected.
    if (nullptr == vptr)
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return Atomese!",
            func.c_str());

    return vptr;
}

// =========== END OF FILE =========
