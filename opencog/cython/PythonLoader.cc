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

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/misc.h>

#include <opencog/atomspace/AtomSpace.h>
#include "PythonEval.h"

// This is an header in the build directory, auto-gened by cython
#include "opencog/atomspace_api.h"

#include <dlfcn.h>

#ifdef __APPLE__
  #define secure_getenv getenv
#endif

using namespace opencog;

const int NO_SIGNAL_HANDLERS = 0;

class GILGuard {
    PyGILState_STATE gstate;
public:
    GILGuard() : gstate(PyGILState_Ensure()) {}
    ~GILGuard() { PyGILState_Release(gstate); }
    GILGuard(const GILGuard&) = delete;
    GILGuard& operator=(const GILGuard&) = delete;
};

// ====================================================
// Python initialization

/**
 * Setup Python sys.path from PYTHONPATH and load Cython API.
 * Python's import system will handle module loading.
 * Return true if the Cython API loaded successfully.
 */
static bool try_to_load_modules()
{
    PyObject* pySysPath = PySys_GetObject((char*)"path");

    // Add PYTHONPATH entries to sys.path
    char* pypath = secure_getenv("PYTHONPATH");
    if (pypath) {
        char* p = strdup(pypath);
        char* token = p;
        char* next = strchr(token, ':');

        while (token) {
            if (next) *next = '\0';

            // Check if it's a valid directory before adding
            struct stat finfo = {};
            if (stat(token, &finfo) == 0 && S_ISDIR(finfo.st_mode)) {
                PyObject* pyModulePath = PyUnicode_DecodeUTF8(
                    token, strlen(token), "strict");
                PyList_Insert(pySysPath, 0, pyModulePath);
                Py_DECREF(pyModulePath);
            }

            token = next ? next + 1 : nullptr;
            if (token) next = strchr(token, ':');
        }
        free(p);
    }

    // NOTE: Can't use get_path_as_string() yet, because it is defined
    // in a Cython api which we can't import, unless the sys.path is
    // correct. So we'll write it out before the imports below to aid
    // in debugging.
    if (logger().is_debug_enabled())
    {
        logger().debug("Python 'sys.path' is:");
        Py_ssize_t pathSize = PyList_Size(pySysPath);
        for (int i = 0; i < pathSize; i++)
        {
            PyObject* pySysPathLine = PyList_GetItem(pySysPath, i);
            PyObject* pyStr = nullptr;
            if (not PyBytes_Check(pySysPathLine)) {
                pyStr = PyUnicode_AsEncodedString(pySysPathLine,
                                               "UTF-8", "strict");
                pySysPathLine = pyStr;
            }
            const char* sysPathCString = PyBytes_AsString(pySysPathLine);
            logger().debug("    %2d > %s", i, sysPathCString);
            // PyList_GetItem returns borrowed reference,
            // so don't do this:
            // Py_DECREF(pySysPathLine);
            if (pyStr) Py_DECREF(pyStr);
        }
    }
    // NOTE: PySys_GetObject returns a borrowed reference so don't do this:
    // Py_DECREF(pySysPath);

    // Initialize the auto-generated Cython api. Do this AFTER the python
    // sys.path is updated so the imports can find the cython modules.
    import_opencog__atomspace();

    // The import_opencog__atomspace() call above sets the
    // py_atomspace() function pointer if the cython module load
    // succeeded. But the function pointer will be NULL if the
    // opencog.atomspace cython module failed to load. Avert
    // a hard-to-debug crash on null-pointer-deref, and replace
    // it by a hard-to-debug error message.
    if (nullptr == py_atomspace) {
        PyErr_Print();
        logger().warn("PythonEval::%s Failed to load the "
                       "opencog.atomspace module", __FUNCTION__);
    }

    return (NULL != py_atomspace);
}

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

    // Calling "import rospy" exhibits bug
    // https://github.com/opencog/atomspace/issues/669
    // Error message:
    //    Python error :
    //    /usr/lib/python2.7/lib-dynload/datetime.x86_64-linux-gnu.so:
    //    undefined symbol: PyExc_SystemError
    // Googling for the above error message reveals that the "feature"
    // is as old as the wind. The solution of using dlopen() is given
    // here:
    // https://mail.python.org/pipermail/new-bugs-announce/2008-November/003322.html

    _dlso = dlopen(PYLIBNAME, RTLD_LAZY | RTLD_GLOBAL);

    logger().info("[global_python_initialize] Start");

    // We don't really know the gstate yet but we'll set it here to avoid
    // compiler warnings below.
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
// Python 3.9 came out in October 2020
#if PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION < 9
        PyEval_InitThreads();
#endif
    }

    logger().info("[global_python_initialize] Adding OpenCog sys.path "
            "directories");

    // Get starting "sys.path".
    PyRun_SimpleString("import sys\n");

    // Setup sys.path and load Cython API
    try_to_load_modules();

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

    // Get sys.path and keep the reference, used in this->add_to_sys_path()
    // NOTE: We have to promote the reference here with Py_INCREF because
    // PySys_GetObject returns a borrowed reference and we don't want it to
    // go away behind the scenes.
    _pySysPath = PySys_GetObject((char*)"path");
    Py_INCREF(_pySysPath);

    // Get the __main__ module. NOTE: As above, PyImport_AddModule returns
    // a borrowed reference so we must promote it with an increment.
    _pyRootModule = PyImport_AddModule("__main__");
    Py_INCREF(_pyRootModule);
    PyModule_AddStringConstant(_pyRootModule, "__file__", "");

    // These are needed for calling Python/C API functions, define
    // them once here so we can reuse them.
    _pyGlobal = PyDict_New();
    _pyLocal = PyDict_New();

    logger().info("PythonEval::%s Finished initialising python evaluator.",
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

void PythonEval::print_dictionary(PyObject* pyDict)
{
    if (!PyDict_Check(pyDict))
        return;

    // Get the keys from the dictionary and print them.
    PyObject* pyKeys = PyDict_Keys(pyDict);
    Py_ssize_t sz = PyList_Size(pyKeys);
    for (int i = 0; i < sz; i++)
    {
        // Get and print one key.
        PyObject* pyKey = PyList_GetItem(pyKeys, i);
        PyObject* pyStr = nullptr;
        if (not PyBytes_Check(pyKey))
        {
            pyStr = PyUnicode_AsEncodedString(pyKey, "UTF-8", "strict");
            Py_DECREF(pyKey);
            pyKey = pyStr;
        }

        const char* c_name = PyBytes_AsString(pyKey);
        printf("Dict item %d is %s\n", i, c_name);

        // PyList_GetItem returns a borrowed reference, so don't do this:
        // Py_DECREF(pyKey);
    }

    // Cleanup the Python reference count for the keys list.
    Py_DECREF(pyKeys);
}

void PythonEval::add_to_sys_path(std::string path)
{
    PyObject* pyPathString = PyBytes_FromString(path.c_str());
    PyList_Append(_pySysPath, pyPathString);

    // We must decrement because, unlike PyList_SetItem, PyList_Append
    // does not "steal" the reference we pass to it. So this:
    //
    // PyList_Append(this->pySysPath, PyBytes_FromString(path.c_str()));
    //
    // leaks memory. So we need to save the reference as above and
    // decrement it, as below.
    //
    Py_DECREF(pyPathString);
}

// ====================================================
// Finding python functions

/**
 * Find the Python object by its name in the given module.
 */
PyObject* PythonEval::find_object(PyObject* pyModule,
                                  const std::string& objectName)
{
    PyObject* pyDict = PyModule_GetDict(pyModule);
    return PyDict_GetItemString(pyDict, objectName.c_str());
}

/**
 * Get the Python module and/or object and stripped function name, given
 * the identifier of the form '[module.][object.[attribute.]*]function'.
 */
PyObject* PythonEval::get_function(const std::string& moduleFunction)
{
    PyObject* pyModule = _pyRootModule;
    PyObject* pyObject = nullptr;
    std::string functionName = moduleFunction;

    // Get the correct module and extract the function name.
    int index = moduleFunction.find_first_of('.');
    if (0 < index)
    {
        std::string moduleName = moduleFunction.substr(0, index);

        // Check Python's sys.modules directly (no need for separate cache)
        PyObject* sys_modules = PyImport_GetModuleDict();
        PyObject* pyModuleTmp = nullptr;

        if (sys_modules)
        {
            pyModuleTmp = PyDict_GetItemString(sys_modules, moduleName.c_str());
        }

        // If not found, first check that it is not an object in __main__.
        // Then try importing it using Python's import system.
        if (nullptr == pyModuleTmp and
            nullptr == find_object(pyModule, moduleName))
        {
            // Try importing the module using Python's standard import
            PyObject* imported = PyImport_ImportModule(moduleName.c_str());
            if (imported) {
                Py_DECREF(imported);  // Module is now in sys.modules

                // Check sys.modules again after import
                if (sys_modules) {
                    pyModuleTmp = PyDict_GetItemString(sys_modules, moduleName.c_str());
                }
            } else {
                // Clear the import error - we'll check if it's an object instead
                PyErr_Clear();
            }
        }

        // If found, set new module and truncate the function name
        if (pyModuleTmp)
        {
            pyModule = pyModuleTmp;
            functionName = moduleFunction.substr(index+1);
        }
    }

    // Iteratively check for objects in the selected (either root
    // or loaded) module.
    index = functionName.find_first_of('.');
    bool bDecRef = false;
    while (0 < index)
    {
        std::string objectName = functionName.substr(0, index);
        // If there is no object yet, find it in Module
        // Else find it as Attr in Object
        if (nullptr == pyObject)
            pyObject = find_object(pyModule, objectName);
        else
        {
            PyObject* pyTmp = PyObject_GetAttrString(pyObject,
                                              objectName.c_str());
            if (bDecRef) Py_DECREF(pyObject);
            pyObject =  pyTmp;
            // Next time, we should use DECREF, since
            // PyObject_GetAttrString returns new reference
            bDecRef = true;
        }

        if (nullptr == pyObject)
            throw RuntimeException(TRACE_INFO,
                "Python object/attribute for '%s' not found!",
                functionName.c_str());

        functionName = functionName.substr(index+1);
        index = functionName.find_first_of('.');
    }

    // For uniformity to DEC later in any case
    if (pyObject && !bDecRef) Py_INCREF(pyObject);

    PyObject* pyUserFunc;

    // If there is no object, then search in module
    if (nullptr == pyObject)
    {
#ifdef DEBUG
        printf("Looking for %s in module %s; here's what we have:\n",
            functionName.c_str(), PyModule_GetName(pyModule));
        print_dictionary(pyDict);
#endif
        PyObject* pyDict = PyModule_GetDict(pyModule);
        pyUserFunc = PyDict_GetItemString(pyDict, functionName.c_str());
    }
    else
        pyUserFunc = PyObject_GetAttrString(pyObject, functionName.c_str());

    // If we can't find that function then throw an exception.
    if (!pyUserFunc)
    {
        if (pyObject) Py_DECREF(pyObject);
        const char * moduleName = PyModule_GetName(pyModule);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' not found in module '%s'!",
            moduleFunction.c_str(), moduleName);
    }

    // Promote the borrowed reference for pyUserFunc since it will
    // be passed to a Python C API function later that "steals" it.
    // PyObject_GetAttrString already returns new reference, so we
    // do this only for PyDict_GetItemString
    if (nullptr == pyObject) Py_INCREF(pyUserFunc);
    if (pyObject) Py_DECREF(pyObject); // We don't need it anymore

    return pyUserFunc;
}

/**
 * Call the user defined function with the arguments passed in the
 * ListLink handle 'arguments'.
 *
 * On error throws an exception.
 */
PyObject* PythonEval::call_user_function(const std::string& moduleFunction,
                                         Handle arguments)
{
    // Get the actual argument count, passed in the ListLink.
    if (arguments->get_type() != LIST_LINK)
        throw RuntimeException(TRACE_INFO,
            "Expecting arguments to be a ListLink!");

    std::lock_guard<std::recursive_mutex> lck(_mtx);

    // Grab the GIL.
    GILGuard gil;

    // Create the Python tuple for the function call with python
    // atoms for each of the atoms in the link arguments.
    size_t nargs = arguments->get_arity();
    PyObject* pyArguments = PyTuple_New(nargs);
    const HandleSeq& args = arguments->getOutgoingSet();
    for (size_t i=0; i<nargs; i++)
        PyTuple_SetItem(pyArguments, i, py_atom(args[i]));

    return do_call_user_function(moduleFunction, pyArguments);
}

// =========== END OF FILE =========
