/*
 * @file opencog/cython/PythonEval.cc
 * @author Zhenhua Cai <czhedu@gmail.com>
 *         Ramin Barati <rekino@gmail.com>
 *         Keyvan Mir Mohammad Sadeghi <keyvan@opencog.org>
 *         Curtis Faith <curtis.m.faith@gmail.com>
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

#include <dlfcn.h>
#include <unistd.h>

#include <boost/filesystem/operations.hpp>

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/misc.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>

#include "PythonEval.h"

#include "opencog/atomspace_api.h"

using std::string;
using std::vector;

using namespace opencog;

//#define DPRINTF printf
#define DPRINTF(...)

#ifdef __APPLE__
  #define secure_getenv getenv
#endif

PythonEval* PythonEval::singletonInstance = NULL;

const int NO_SIGNAL_HANDLERS = 0;
const char* NO_FUNCTION_NAME = NULL;
const int SIMPLE_STRING_SUCCESS = 0;
const int SIMPLE_STRING_FAILURE = -1;
const int MISSING_FUNC_CODE = -1;

// The Python functions can't take const flags.
static bool already_initialized = false;
static bool initialized_outside_opencog = false;
std::recursive_mutex PythonEval::_mtx;

/*
 * @todo When can we remove the singleton instance? Answer: not sure.
 * Python itself is single-threaded. That is, currently, python fakes
 * multi-thread support by grabbing a lock and only allowing just
 * one thread to run at a time.  Our singleton instance mirrors this
 * python limitation.  However, if we could set a per-thread atomspace,
 * then we could maybe still have python be callable in mutiple threads,
 * with different atomspaces. Clearly python would become a major
 * bottleneck, but this is why we strongly discourage using python.
 *
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

static const char* DEFAULT_PYTHON_MODULE_PATHS[] =
{
    DATADIR"/python",                    // install directory
    #ifndef WIN32
    "/usr/local/share/opencog/python",
    "/usr/share/opencog/python",
    #endif // !WIN32
    NULL
};

static const char* PROJECT_PYTHON_MODULE_PATHS[] =
{
    PROJECT_BINARY_DIR"/opencog/cython", // bindings
    PROJECT_SOURCE_DIR"/opencog/python", // opencog modules written in python
    PROJECT_SOURCE_DIR"/tests/cython",   // for testing
    NULL
};

// Weird hack to work-around nutty python behavior.  Python sucks.
// I spent three effing 12 hour-days tracking this down. Cython sucks.
// The python bug this is working around is this: python does not
// know how to load modules unless there is an __init__.py in the
// directory. However, once it finds this, then it stops looking,
// and just assumes that everything is in that directory. Of course,
// this is a bad assumption: python modules can be in a variety of
// directories. In particular, they can be in the local build
// directories.  If we put the local build directories in the search
// path first, then the modules are found. But if we then install this
// code into the system (i.e. into /usr or /usr/local) then python will
// still look in these build directories, even though they are bogus
// when the system version is installed. This, of course, results in the
// build directories "hiding" the install directories, causing only
// some, but not all of the modules to be found. This ends up being
// terribly confusing, because you get "ImportError: No module named
// blah" errors, even though you can be staring the module right in the
// face, and there it is!
//
// So the ugly work-around implemented here is this: If code is
// executing in the project source or project build directories, then
// add the project source and build directories to the search path, and
// place them first, so that they are searched before the system
// directories. Otherwise, do not search the build directories.
// This is just plain fucked up, but I cannot find a better solution.
static const char** get_module_paths()
{
    static const char** paths = nullptr;

    if (paths) return paths;

    unsigned nproj = sizeof(PROJECT_PYTHON_MODULE_PATHS) / sizeof(char**);
    unsigned ndefp = sizeof(DEFAULT_PYTHON_MODULE_PATHS) / sizeof(char**);

    unsigned nenv = 0;
    char* pypath = secure_getenv("PYTHONPATH");
    if (pypath) {
        char *p = pypath;
        while (p) { p = strchr(p+1, ':'); nenv++; }
    }

    paths = (const char**) malloc(sizeof(char *) * (nproj + ndefp + nenv + 1));

    // Get current working directory.
    char* cwd = getcwd(NULL, 0);
    bool in_project = false;
    if (0 == strncmp(PROJECT_SOURCE_DIR, cwd, strlen(PROJECT_SOURCE_DIR)) or
        0 == strncmp(PROJECT_BINARY_DIR, cwd, strlen(PROJECT_BINARY_DIR)))
        in_project = true;
    free(cwd);

    // If the currrent working directory is the projet build or source
    // directory, then search those first.
    int ip = 0;
    if (in_project) {
        for (unsigned i=0; i < nproj-1; i++) {
            paths[ip] = PROJECT_PYTHON_MODULE_PATHS[i];
            ip++;
        }
    }

    // Search the usual locations next.
    for (unsigned i=0; i < ndefp-1; i++) {
        paths[ip] = DEFAULT_PYTHON_MODULE_PATHS[i];
        ip++;
    }

    // Finally, use the environment variable.
    if (pypath) {
        char* p = strdup(pypath);
        char* q = strchr(p, ':');
        while (true) {
           if (q) *q = '\0';
           paths[ip] = p;
           ip++;
           if (nullptr == q) break;
           p = q+1;
           q = strchr(p, ':');
       }
    }
    paths[ip] = NULL;

    return paths;
}


/**
 * Ongoing python nuttiness. Because we never know in advance whether
 * python will be able to find a module or not, until it actually does,
 * we have to proceed by trial and error, trying different path
 * combinations, until it finally works.  The sequence of paths
 * that leads to success will then be delcared the official system
 * path, henceforth. Woe unto those try to defy the will of the python
 * gods.
 *
 * Return true if the load worked, else return false.
 */
static bool try_to_load_modules(const char ** config_paths)
{
    PyObject* pySysPath = PySys_GetObject((char*)"path");

    // Add default OpenCog module directories to the Python interpreter's path.
    for (int i = 0; config_paths[i] != NULL; ++i)
    {
        struct stat finfo = {};
        stat(config_paths[i], &finfo);

        if (S_ISDIR(finfo.st_mode))
        {
#if PY_MAJOR_VERSION < 3
            PyObject* pyModulePath = PyBytes_FromString(config_paths[i]);
#else
            PyObject* pyModulePath = PyUnicode_DecodeUTF8(
                  config_paths[i], strlen(config_paths[i]), "strict");
#endif
            PyList_Append(pySysPath, pyModulePath);
            Py_DECREF(pyModulePath);
        }
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
    // opencopg.atomspace cython module failed to load. Avert
    // a hard-to-debug crash on null-pointer-deref, and replace
    // it by a hard-to-debug error message.
    if (nullptr == py_atomspace) {
        PyErr_Print();
        logger().warn("PythonEval::%s Failed to load the "
                       "opencog.atomspace module", __FUNCTION__);
    }

    return (NULL != py_atomspace);
}

void opencog::global_python_initialize()
{
    // Calling "import rospy" exhibits bug
    // https://github.com/opencog/atomspace/issues/669
    // Error message:
    //    Python error :
    //    /usr/lib/python2.7/lib-dynload/datetime.x86_64-linux-gnu.so:
    //    undefined symbol: PyExc_SystemError
    // Googling for the above error messge reveals that the "feature"
    // is as old as the wind. The solution of using dlopen() is given
    // here:
    // https://mail.python.org/pipermail/new-bugs-announce/2008-November/003322.html
#if PY_MAJOR_VERSION < 3
    dlopen("libpython2.7.so", RTLD_LAZY | RTLD_GLOBAL);
#else
    dlopen("libpython3.5.so", RTLD_LAZY | RTLD_GLOBAL);
#endif

    logger().info("[global_python_initialize] Start");

    // Don't initialize twice
    if (already_initialized) {
        return;
    }

    // Remember this initialization.
    already_initialized = true;

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
        PyEval_InitThreads();

        // Many python libraries (e.g. ROS) expect sys.argv to be set.
        // So, avoid the error print, and let them know who we are.
        // We must do this *before* the module pre-loading, done below.
        PyRun_SimpleString("import sys; sys.argv='cogserver'\n");
    }

    logger().info("[global_python_initialize] Adding OpenCog sys.path "
            "directories");

    // Get starting "sys.path".
    PyRun_SimpleString("import sys\n");

    // Add default OpenCog module directories to the Python interprator's path.
    try_to_load_modules(get_module_paths());

    // Hmm. If the above returned false, we should try a different
    // permuation of the config paths.  I'm confused, though, different
    // users are reporting conflicting symptoms.  What to do?

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

    // Cleanup Python.
    if (!initialized_outside_opencog)
    {
        PyGILState_Ensure(); // yes this is needed, see bug #671
        Py_Finalize();
    }

    // No longer initialized.
    already_initialized = false;

    logger().debug("[global_python_finalize] Finish");
}

PythonEval::PythonEval(AtomSpace* atomspace)
{
    // Check that this is the first and only PythonEval object.
    if (singletonInstance) {
        throw RuntimeException(TRACE_INFO,
            "Can't create more than one PythonEval singleton instance!");
    }

    // Remember our atomspace.
    _atomspace = atomspace;
    _paren_count = 0;

    // Initialize Python objects and imports.
    //
    // Strange but true: one can use the atomspace, and put atoms
    // in it .. using the type constructors and everything (e.g.
    // the demos in the /examples/python directory) and never ever
    // actually call global_python_initialize() ... it might never
    // be called, if the python evaluator (i.e. this object) is
    // never used or needed.  I thought this was unexpected, so I
    // mention it here.
    global_python_initialize();
    this->initialize_python_objects_and_imports();
}

PythonEval::~PythonEval()
{
    logger().info("PythonEval::%s destructor", __FUNCTION__);

    // Grab the GIL
    PyGILState_STATE gstate;
    gstate = PyGILState_Ensure();

    // Decrement reference counts for instance Python object references.
    Py_DECREF(_pyGlobal);
    Py_DECREF(_pyLocal);

    // NOTE: The following come from Python C api calls that return borrowed
    // references. However, we have called Py_INCREF( x ) to promote them
    // to full references so we can and must decrement them here.
    Py_DECREF(_pySysPath);
    Py_DECREF(_pyRootModule);

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);
}

/**
* Use a singleton instance to avoid initializing python interpreter twice.
*/
void PythonEval::create_singleton_instance(AtomSpace* atomspace)
{
    if (singletonInstance) return;

    // Create the single instance of a PythonEval object.
    singletonInstance = new PythonEval(atomspace);
}

void PythonEval::delete_singleton_instance()
{
    if (!singletonInstance) return;

    // Delete the singleton PythonEval instance.
    delete singletonInstance;
    singletonInstance = NULL;
}

PythonEval& PythonEval::instance(AtomSpace* atomspace)
{
    // Make sure we have a singleton.
    if (!singletonInstance)
        create_singleton_instance(atomspace);

    // Make sure the atom space is the same as the one in the singleton.
    if (atomspace and singletonInstance->_atomspace != atomspace) {

#define CHECK_SINGLETON
#ifdef CHECK_SINGLETON
        if (nullptr != singletonInstance->_atomspace)
        {
            // Someone is trying to initialize the Python interpreter on a
            // different AtomSpace.  Because of the singleton design of the
            // the CosgServer+AtomSpace, there is no easy way to support this...
            // logger().error() will print a stack tace to tell use who
            // is doing this.
            logger().error("PythonEval: ",
                "Trying to re-initialize python interpreter with different\n"
                "AtomSpace ptr! Current ptr=%p uuid=%d "
                "New ptr=%p uuid=%d\n",
                singletonInstance->_atomspace,
                singletonInstance->_atomspace->get_uuid(),
                atomspace, atomspace?atomspace->get_uuid():0);

            throw RuntimeException(TRACE_INFO,
                "Trying to re-initialize python interpreter with different\n"
                "AtomSpace ptr! Current ptr=%p New ptr=%p\n",
                singletonInstance->_atomspace, atomspace);
        }
#else
        // We need to be able to call the python interpreter with
        // different atomspaces; for example, we need to use temporary
        // atomspaces when evaluating virtual links.  So, just set it
        // here.  Hopefully the user will set it back, after using the
        // temp atomspace.   Cleary, this is not thread-safe, and will
        // bust with multiple threads. But the whole singleton-instance
        // design is fundamentally flawed, so there is not much we can
        // do about it until someone takes the time to fix this class
        // to allow multiple instances.
        //
        singletonInstance->_atomspace = atomspace;
#endif
    }
    return *singletonInstance;
}

void PythonEval::initialize_python_objects_and_imports(void)
{
    // Grab the GIL
    PyGILState_STATE gstate;
    gstate = PyGILState_Ensure();

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

#define SET_ATOMSPACE_IN_MODULE
#ifdef SET_ATOMSPACE_IN_MODULE
    // This seems like a really bad idea ... why would we do this?
    // Add ATOMSPACE to __main__ module.
    PyObject* pyRootDictionary = PyModule_GetDict(_pyRootModule);
    PyObject* pyAtomSpaceObject = this->atomspace_py_object(_atomspace);

    // Sometimes the atomspace cannot be found, viz null pointer.
    // I don't know why.
    if (pyAtomSpaceObject)
    {
        PyDict_SetItemString(pyRootDictionary, "ATOMSPACE", pyAtomSpaceObject);
        Py_DECREF(pyAtomSpaceObject);
    }

    // PyModule_GetDict returns a borrowed reference, so don't do this:
    // Py_DECREF(pyRootDictionary);
#endif // SET_ATOMSPACE_IN_MODULE

    if (nullptr == _atomspace)
        logger().warn("Python evaluator initialized with null atomspace!");

    // These are needed for calling Python/C API functions, define
    // them once here so we can reuse them.
    _pyGlobal = PyDict_New();
    _pyLocal = PyDict_New();

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);

    logger().info("PythonEval::%s Finished initialising python evaluator.",
        __FUNCTION__);
}

PyObject* PythonEval::atomspace_py_object(AtomSpace* atomspace)
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

/***********
    Weird ... I guess NULL atomspaces are OK!?
    if (NULL == atomspace) {
        logger().warn("PythonEval::%s No atomspace specified!",
                       __FUNCTION__);
        return NULL;
    }
************/

    PyObject * pyAtomSpace = py_atomspace(atomspace);

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
            pyKey = pyStr;
        }
        if (pyStr) Py_DECREF(pyStr);

        const char* c_name = PyBytes_AsString(pyKey);
        printf("Dict item %d is %s\n", i, c_name);

        // PyList_GetItem returns a borrowed reference, so don't do this:
        // Py_DECREF(pyKey);
    }

    // Cleanup the Python reference count for the keys list.
    Py_DECREF(pyKeys);
}

/**
 * Build the Python error message for the current error.
 *
 * Only call this when PyErr_Occurred() returns a non-null PyObject*.
 */
void PythonEval::build_python_error_message(const char* function_name,
                                            std::string& errorMessage)
{
    PyObject *pyErrorType, *pyError, *pyTraceback, *pyErrorString;
    std::stringstream errorStringStream;

    // Get the error from Python.
    PyErr_Fetch(&pyErrorType, &pyError, &pyTraceback);

    // Construct the error message string.
    errorStringStream << "Python error ";
    if (function_name != NO_FUNCTION_NAME)
        errorStringStream << "in " << function_name;
    if (pyError) {
        pyErrorString = PyObject_Str(pyError);
        char* pythonErrorString = PyBytes_AS_STRING(pyErrorString);
        if (pythonErrorString) {
            errorStringStream << ": " << pythonErrorString << ".";
        } else {
            errorStringStream << ": Undefined Error";
        }

        // Cleanup the references. NOTE: The traceback can be NULL even
        // when the others aren't.
        Py_DECREF(pyErrorType);
        Py_DECREF(pyError);
        Py_DECREF(pyErrorString);
        if (pyTraceback)
            Py_DECREF(pyTraceback);

    } else {
        errorStringStream << ": Undefined Error";
    }
    errorMessage = errorStringStream.str();
}

/**
 * Execute the python string at the __main__ module context.
 *
 * This replaces a call to PyRun_SimpleString which clears errors so
 * that a subsequent call to Py_Error() returns false. This version
 * does everything that PyRun_SimpleString does except it does not
 * call PyErr_Print() and PyErr_Clear().
 */
void PythonEval::execute_string(const char* command)
{
    // We use Py_file_input here, instead of Py_single_input, because
    // Py_single_input spews errors on blank lines.  However, the
    // flip-side is that simple expressions, such as 2+2, generate no
    // output at all when Py_file_input is used; they do generate output
    // when Py_single_input is used.
    //
    // In either case, the pyResult does not have any string
    // repesentation; using PyObject_Str(pyResult) and then
    // PyString_AsString to print it gives "None" in all situations.
    // Because of this, I don't know how to write a valid command
    // interpreter for the python shell ...
    PyObject* pyRootDictionary = PyModule_GetDict(_pyRootModule);
    PyObject* pyResult = PyRun_StringFlags(command,
            Py_file_input, pyRootDictionary, pyRootDictionary,
            nullptr);

    if (pyResult)
        Py_DECREF(pyResult);

    PyObject *f = PySys_GetObject((char *) "stdout");
    if (f) fsync(PyObject_AsFileDescriptor(f));  // Force a flush
}

int PythonEval::argument_count(PyObject* pyFunction)
{
    PyObject* pyFunctionCode;
    PyObject* pyArgumentCount;
    int argumentCount;

    // Get the 'function.func_code.co_argcount' Python internal attribute.
    pyFunctionCode = PyObject_GetAttrString(pyFunction, "__code__");
    if (pyFunctionCode) {
        pyArgumentCount = PyObject_GetAttrString(pyFunctionCode, "co_argcount");
        if (pyArgumentCount) {
            argumentCount = PyLong_AsLong(pyArgumentCount);
        }  else {
            Py_DECREF(pyFunctionCode);
            return MISSING_FUNC_CODE;
        }
    } else {
        return MISSING_FUNC_CODE;
    }

    // Cleanup the reference counts.
    Py_DECREF(pyFunctionCode);
    Py_DECREF(pyArgumentCount);

    return argumentCount;
}

/**
 * Get the Python module and stripped function name given the identifer of
 * the form 'module.function'.
 */
PyObject* PythonEval::module_for_function(const std::string& moduleFunction,
                                          std::string& functionName)
{
    // Get the correct module and extract the function name.
    int index = moduleFunction.find_first_of('.');
    if (0 < index) {
        std::string moduleName = moduleFunction.substr(0, index);
        PyObject* pyModule = _modules[moduleName];

        // If not found, try loading it.
        // We have to guess, if its a single file, or an entire
        // directory with an __init__.py file in it ...
        if (nullptr == pyModule) {
            add_modules_from_path(moduleName);
            add_modules_from_path(moduleName + ".py");
            pyModule = _modules[moduleName];
        }

        // If found, we are done.
        if (pyModule) {
            functionName = moduleFunction.substr(index+1);
            return pyModule;
        }
    }
    functionName = moduleFunction;
    return _pyRootModule;
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
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    // Grab the GIL.
    PyGILState_STATE gstate = PyGILState_Ensure();

    // Get the module and stripped function name.
    std::string functionName;
    PyObject* pyModule = this->module_for_function(moduleFunction, functionName);

    // If we can't find that module then throw an exception.
    if (!pyModule) {
        PyGILState_Release(gstate);
        logger().warn("Python module for '%s' not found!", moduleFunction.c_str());
        throw RuntimeException(TRACE_INFO,
            "Python module for '%s' not found!",
            moduleFunction.c_str());
    }

    // Get a reference to the user function.
    PyObject* pyDict = PyModule_GetDict(pyModule);

// #define DEBUG
#ifdef DEBUG
    printf("Looking for %s in module %s; here's what we have:\n",
        functionName.c_str(), moduleFunction.c_str());
    print_dictionary(pyDict);
#endif

    PyObject* pyUserFunc = PyDict_GetItemString(pyDict, functionName.c_str());

    // PyModule_GetDict returns a borrowed reference, so don't do this:
    // Py_DECREF(pyDict);

    // If we can't find that function then throw an exception.
    if (!pyUserFunc) {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' not found!",
            moduleFunction.c_str());
    }

    // Promote the borrowed reference for pyUserFunc since it will
    // be passed to a Python C API function later that "steals" it.
    Py_INCREF(pyUserFunc);

    // Make sure the function is callable.
    if (!PyCallable_Check(pyUserFunc)) {
        Py_DECREF(pyUserFunc);
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' not callable!", moduleFunction.c_str());
    }

    // Get the expected argument count.
    int expectedArgumentCount = this->argument_count(pyUserFunc);
    if (expectedArgumentCount == MISSING_FUNC_CODE) {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' error missing 'func_code'!",
            moduleFunction.c_str());
    }

    // Get the actual argument count, passed in the ListLink.
    if (arguments->get_type() != LIST_LINK) {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Expecting arguments to be a ListLink!");
    }

    // Now make sure the expected count matches the actual argument count.
    int actualArgumentCount = arguments->get_arity();
    if (expectedArgumentCount != actualArgumentCount) {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' which expects '%d arguments,"
            " called with %d arguments!", moduleFunction.c_str(),
            expectedArgumentCount, actualArgumentCount);
    }

    // Create the Python tuple for the function call with python
    // atoms for each of the atoms in the link arguments.
    PyObject* pyArguments = PyTuple_New(actualArgumentCount);
    PyObject* pyAtomSpace = this->atomspace_py_object(_atomspace);
    const HandleSeq& argumentHandles = arguments->getOutgoingSet();
    int tupleItem = 0;
    for (const Handle& h: argumentHandles)
    {
        // Place a Python atom object for this handle into the tuple.

        long int patom = (long int) &h;
        PyObject* pyAtom = py_atom(patom, pyAtomSpace);
        PyTuple_SetItem(pyArguments, tupleItem, pyAtom);

        // PyTuple_SetItem steals it's item so don't do this:
        // Py_DECREF(pyAtom)

        ++tupleItem;
    }
    Py_DECREF(pyAtomSpace);

    // Execute the user function and store its return value.
    PyObject* pyReturnValue = PyObject_CallObject(pyUserFunc, pyArguments);

    // Cleanup the reference counts for Python objects we no longer reference.
    // Since we promoted the borrowed pyExecuteUserFunc reference, we need
    // to decrement it here. Do this before error checking below since we'll
    // need to decrement these references even if there is an error.
    Py_DECREF(pyUserFunc);
    Py_DECREF(pyArguments);

    // Check for errors.
    if (PyErr_Occurred())
    {
        // Construct the error message and throw an exception.
        std::string errorString;
        this->build_python_error_message(moduleFunction.c_str(), errorString);
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO, "%s", errorString.c_str());

        // PyErr_Occurred returns a borrowed reference, so don't do this:
        // Py_DECREF(pyError);
    }

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);

    return pyReturnValue;
}

Handle PythonEval::apply(AtomSpace* as, const std::string& func, Handle varargs)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    RAII raii(this, as);

    // Get the atom object returned by this user function.
    PyObject* pyReturnAtom = this->call_user_function(func, varargs);

    // If we got a non-null atom were no errors.
    if (pyReturnAtom)
    {
        // Grab the GIL.
        PyGILState_STATE gstate;
        gstate = PyGILState_Ensure();

        // Get the handle from the atom.
        PyObject* pyAtomPATOM = PyObject_CallMethod(pyReturnAtom,
                (char*) "handle_ptr", NULL);

        // Make sure we got an atom pointer.
        PyObject* pyError = PyErr_Occurred();
        if (pyError or nullptr == pyAtomPATOM)
        {
            PyGILState_Release(gstate);
            throw RuntimeException(TRACE_INFO,
                "Python function '%s' did not return Atom!", func.c_str());
        }

        // Get the atom pointer from the python atom pointer.
        // Save it, because the DECREF will blow it away.
        Handle hresult = *((Handle*)(PyLong_AsLong(pyAtomPATOM)));

        // Cleanup the reference counts.
        Py_DECREF(pyReturnAtom);
        Py_DECREF(pyAtomPATOM);

        // Release the GIL. No Python API allowed beyond this point.
        PyGILState_Release(gstate);
        return hresult;
    }
    else
    {
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return Atom!", func.c_str());
    }

    return Handle::UNDEFINED;
}

/**
 * Apply the user function to the arguments passed in varargs and
 * return the extracted truth value.
 */
TruthValuePtr PythonEval::apply_tv(AtomSpace *as,
                                   const std::string& func,
                                   Handle varargs)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    RAII raii(this, as);

    // Get the python truth value object returned by this user function.
    PyObject *pyTruthValue = call_user_function(func, varargs);

    // If we got a non-null truth value there were no errors.
    if (NULL == pyTruthValue)
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return TruthValue!",
            func.c_str());

    // Grab the GIL.
    PyGILState_STATE gstate = PyGILState_Ensure();

    // Get the truth value pointer from the object (will be encoded
    // as a long by PyVoidPtr_asLong)
    PyObject *pyTruthValuePtrPtr = PyObject_CallMethod(pyTruthValue,
            (char*) "truth_value_ptr_object", NULL);

    // Make sure we got a truth value pointer.
    PyObject *pyError = PyErr_Occurred();
    if (pyError or !pyTruthValuePtrPtr)
    {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return TruthValue!",
            func.c_str());
    }

    // Get the pointer to the truth value pointer. Yes, it does
    // contain a pointer to the shared_ptr not the underlying.
    TruthValuePtr* tvpPtr = static_cast<TruthValuePtr*>
            (PyLong_AsVoidPtr(pyTruthValuePtrPtr));

    // Assign the truth value pointer using this pointer before
    // we decrement the reference to pyTruthValue since that
    // will delete this pointer.
    TruthValuePtr tvp = *tvpPtr;

    // Cleanup the reference counts.
    Py_DECREF(pyTruthValuePtrPtr);
    Py_DECREF(pyTruthValue);

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);
    return tvp;
}

/**
 * Call the user defined function with the provide atomspace argument.
 * This is a cut-n-paste of PythonEval::call_user_function but with
 * assorted hacks to handle the different argument type.
 *
 * On error throws an exception.
 */
void PythonEval::apply_as(const std::string& moduleFunction,
                          AtomSpace* as_argument)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    PyObject *pyError, *pyModule, *pyUserFunc;
    PyObject *pyDict;
    std::string functionName;
    std::string errorString;

    // Grab the GIL.
    PyGILState_STATE gstate;
    gstate = PyGILState_Ensure();

    // Get the module and stripped function name.
    pyModule = this->module_for_function(moduleFunction, functionName);

    // If we can't find that module then throw an exception.
    if (!pyModule)
    {
        PyGILState_Release(gstate);
        logger().warn("Python module for '%s' not found!",
                      moduleFunction.c_str());
        throw RuntimeException(TRACE_INFO,
            "Python module for '%s' not found!",
            moduleFunction.c_str());
    }

    // Get a reference to the user function.
    pyDict = PyModule_GetDict(pyModule);
    pyUserFunc = PyDict_GetItemString(pyDict, functionName.c_str());

    // PyModule_GetDict returns a borrowed reference, so don't do this:
    // Py_DECREF(pyDict);

    // If we can't find that function then throw an exception.
    if (!pyUserFunc) {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' not found!",
            moduleFunction.c_str());
    }

    // Promote the borrowed reference for pyUserFunc since it will
    // be passed to a Python C API function later that "steals" it.
    Py_INCREF(pyUserFunc);

    // Make sure the function is callable.
    if (!PyCallable_Check(pyUserFunc))
    {
        Py_DECREF(pyUserFunc);
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' not callable!", moduleFunction.c_str());
    }

    // Get the expected argument count.
    int expectedArgumentCount = this->argument_count(pyUserFunc);
    if (expectedArgumentCount == MISSING_FUNC_CODE)
    {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' error missing 'func_code'!",
            moduleFunction.c_str());
    }

    // Make sure the argument count is 1 (just the atomspace)
    if (1 != expectedArgumentCount)
    {
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' which expects '%d arguments,"
            " should have one atomspace argument!",
            moduleFunction.c_str(), expectedArgumentCount);
    }

    // Create the Python tuple for the function call with python
    // atomspace.
    PyObject* pyArguments = PyTuple_New(1);
    PyObject* pyAtomSpace = this->atomspace_py_object(as_argument);

    PyTuple_SetItem(pyArguments, 0, pyAtomSpace);
    // Py_DECREF(pyAtomSpace);

    // Execute the user function.
    PyObject_CallObject(pyUserFunc, pyArguments);

    // Cleanup the reference counts for Python objects we no longer reference.
    // Since we promoted the borrowed pyExecuteUserFunc reference, we need
    // to decrement it here. Do this before error checking below since we'll
    // need to decrement these references even if there is an error.
    Py_DECREF(pyUserFunc);
    Py_DECREF(pyArguments);

    // Check for errors.
    pyError = PyErr_Occurred();
    if (pyError)
    {
        // Construct the error message and throw an exception.
        this->build_python_error_message(moduleFunction.c_str(), errorString);
        PyGILState_Release(gstate);
        throw RuntimeException(TRACE_INFO, "%s", errorString.c_str());

        // PyErr_Occurred returns a borrowed reference, so don't do this:
        // Py_DECREF(pyError);
    }

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);
}

std::string PythonEval::apply_script(const std::string& script)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    // Grab the GIL
    PyGILState_STATE gstate = PyGILState_Ensure();

    // Execute the script. NOTE: This call replaces PyRun_SimpleString
    // which was masking errors because it calls PyErr_Clear() so the
    // call to PyErr_Occurred below was returning false even when there
    // was an error.
    this->execute_string(script.c_str());

    bool errorRunningScript = false;
    std::string errorString;

    // Check for errors in the script.
    if (PyErr_Occurred())
    {
        // Remember the error and get the error string for the throw below.
        errorRunningScript = true;
        this->build_python_error_message(NO_FUNCTION_NAME, errorString);
    }

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);

    // If there was an error, throw an exception so the user knows the
    // script had a problem.
    if (errorRunningScript)
        throw RuntimeException(TRACE_INFO, "%s", errorString.c_str());

    return "";
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

const int ABSOLUTE_IMPORTS_ONLY = 0;

void PythonEval::import_module(const boost::filesystem::path &file,
                               PyObject* pyFromList)
{
    // The pyFromList parameter corresponds to what would appear in
    // an import statement after the import:
    //
    // from <module> import <from list>
    //
    // When this list is empty, this corresponds to an import of the
    // entire module as is done in the simple import statement:
    //
    // import <module>

    // Get the module name from the Python file name by removing the ".py"
    std::string fileName = file.filename().c_str();
    std::string moduleName = fileName.substr(0, fileName.length()-3);

    logger().info("    importing Python module: " + moduleName);

    // Import the entire module into the current Python environment.
    PyObject* pyModule = PyImport_ImportModuleLevel((char*) moduleName.c_str(),
            _pyGlobal, _pyLocal, pyFromList,
            ABSOLUTE_IMPORTS_ONLY);

    if (nullptr == pyModule)
    {
        if (PyErr_Occurred()) PyErr_Print();
        logger().warn() << "Couldn't import '" << moduleName << "' module";
        return;
    }

#ifdef SET_ATOMSPACE_IN_MODULE
    // This seems like a really bad idea ... why would we do this?
    PyObject* pyModuleDictionary = PyModule_GetDict(pyModule);

    // Add the ATOMSPACE object to this module
    PyObject* pyAtomSpaceObject = this->atomspace_py_object(_atomspace);
    PyDict_SetItemString(pyModuleDictionary, "ATOMSPACE",
            pyAtomSpaceObject);

    // This decrement is needed because PyDict_SetItemString does
    // not "steal" the reference, unlike PyList_SetItem.
    Py_DECREF(pyAtomSpaceObject);
    if (nullptr == _atomspace)
        logger().warn("Python module initialized with null atomspace!");
#endif // SET_ATOMSPACE_IN_MODULE

    // We need to increment the pyModule reference because
    // PyModule_AddObject "steals" it and we're keeping a copy
    // in our modules list.
    Py_INCREF(pyModule);

    // Add the module name to the root module.
    PyModule_AddObject(_pyRootModule, moduleName.c_str(), pyModule);

    // Add the module to our modules list. So don't decrement the
    // Python reference in this function.
    _modules[moduleName] = pyModule;
}

/**
* Add all the .py files in the given directory as modules to __main__ and
* keep the references in a dictionary (this->modules)
*/
void PythonEval::add_module_directory(const boost::filesystem::path &directory)
{
    vector<boost::filesystem::path> files;
    vector<boost::filesystem::path> pyFiles;

    // Loop over the files in the directory looking for Python files.
    copy(boost::filesystem::directory_iterator(directory),
         boost::filesystem::directory_iterator(), back_inserter(files));

    for (vector<boost::filesystem::path>::const_iterator it(files.begin());
            it != files.end(); ++it)
    {
        if (it->extension() == boost::filesystem::path(".py"))
            pyFiles.push_back(*it);
    }

    // Add the directory we are adding to Python's sys.path
    this->add_to_sys_path(directory.c_str());

    // The pyFromList variable corresponds to what would appear in
    // an import statement after the import:
    //
    // from <module> import <from list>
    //
    // When this list is empty, as below, this corresponds to an import
    // of the entire module as is done in the simple import statement:
    //
    // import <module>
    //
    PyObject* pyFromList = PyList_New(0);

    // Import each of the ".py" files as a Python module.
    for (vector<boost::filesystem::path>::const_iterator it(pyFiles.begin());
            it != pyFiles.end(); ++it)
        this->import_module(*it, pyFromList);

    // Cleanup the reference count for the from list.
    Py_DECREF(pyFromList);
}

/**
* Add the .py file in the given path as a module to __main__ and add the
* reference to the dictionary (this->modules)
*/
void PythonEval::add_module_file(const boost::filesystem::path &file)
{
    // Add this file's parent path to sys.path so Python imports
    // can find it.
    this->add_to_sys_path(file.parent_path().c_str());

    // The pyFromList variable corresponds to what would appear in
    // an import statement after the import:
    //
    // from <module> import <from list>
    //
    // When this list is empty, as below, this corresponds to an import
    // of the entire module as is done in the simple import statement:
    //
    // import <module>

    // Import this file as a module.
    PyObject* pyFromList = PyList_New(0);
    this->import_module(file, pyFromList);
    Py_DECREF(pyFromList);
}

/**
 * Get a path and determine if it is a file or directory, then call the
 * corresponding function specific to directories and files.
 */
void PythonEval::add_modules_from_path(std::string pathString)
{
    std::vector<std::string> dirs;
    std::vector<std::string> files;
    bool found = false;

    auto loadmod_prep = [&dirs, &files, &found](const std::string& abspath,
            const char** config_paths)
    {
        // If the resulting path is a directory or a regular file,
        // then push to loading list.
        struct stat finfo;
        stat(abspath.c_str(), &finfo);
        if (S_ISDIR(finfo.st_mode)) {
            found = true;
            dirs.push_back(abspath);
            logger().info() << "Found python module in directory \'"
                << abspath << "\'";
        }

        else if (S_ISREG(finfo.st_mode)) {
            found = true;
            files.push_back(abspath);
            logger().info() << "Found python module in file \'"
                << abspath << "\'";
        }
    };

    const char** config_paths = get_module_paths();
    std::vector<std::string> paths;
    tokenize(pathString, std::back_inserter(paths), ",");
    for (const auto& pathString : paths)
    {
        if ('/' == pathString[0]) {
            loadmod_prep(pathString, NULL);
            continue;
        }

        else if ('.' == pathString[0]) {
            boost::filesystem::path base(getcwd(NULL, 0));
            auto pypath = boost::filesystem::canonical(pathString, base);
            loadmod_prep(pypath.string(), NULL);
            continue;

        } else {
            for (int i = 0; config_paths[i] != NULL; ++i) {
                std::string abspath = config_paths[i];
                abspath += "/";
                abspath += pathString;
                loadmod_prep(abspath, config_paths);
            }
        }
    }

    if (not found)
    {
        Logger::Level btl = logger().get_backtrace_level();
        logger().set_backtrace_level(Logger::Level::NONE);
        logger().warn() << "Failed to load python module \'"
            << pathString << "\', searched directories:";
        for (int i = 0; config_paths[i] != NULL; ++i) {
            logger().warn() << "Directory: " << config_paths[i];
        }
        logger().set_backtrace_level(btl);
    }

    // First, load directories, and then load files, to properly
    // handle import dependencies.
    dirs.insert(dirs.end(), files.begin(), files.end());
    for (const auto& abspath : dirs)
        add_modules_from_abspath(abspath);
}

void PythonEval::add_modules_from_abspath(std::string pathString)
{
    logger().info("Adding Python module (or directory): " + pathString);

    // Grab the GIL
    PyGILState_STATE gstate;
    gstate = PyGILState_Ensure();

    struct stat finfo;
    stat(pathString.c_str(), &finfo);

    if (S_ISDIR(finfo.st_mode))
        add_module_directory(pathString);
    else if (S_ISREG(finfo.st_mode))
        add_module_file(pathString);
    else
        logger().warn() << "Python module path \'" << pathString
                        << "\' can't be found";

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);
}

void PythonEval::begin_eval()
{
    _eval_done = false;
    _result = "";
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

/// Like eval_expr(), except that it assumes that there is only
/// one line per call, i.e. that partial expr has been split up
/// into lines.
//
// The python interpreter chokes if we send it lines, instead of
// blocks. Thus, we have to save up whole blocks.  A block consists
// of:
//
// 1) Something that starts unindented, and continues until the
//    start of the next non-comment unindented line, or until
//    end-of-file.
// 2) Anything surrounded by parenthesis, regardless of indentation.
//
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
        size_t clos = std::count(part.begin(), part.end(), ')');
        _paren_count += open - clos;
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

    // This is the cogserver shell-freindly evaluator. We must
    // stop all exceptions thrown in other layers, or else we
    // will crash the cogserver. Pass the exception message to
    // the user, who can read and contemplate it: it is almost
    // surely a syntax error in the python code.
    try
    {
        this->apply_script(_input_line);
    }
    catch (const RuntimeException &e)
    {
        _result += e.get_message();
        _result += "\n";
    }
    _input_line = "";
    _paren_count = 0;
    _pending_input = false;
    logger().debug("[PythonEval] eval_expr result length=%zu:\n%s",
                  _result.length(), _result.c_str());

    _eval_done = true;
    _wait_done.notify_all();
    return;

wait_for_more:
    _pending_input = true;
    // Add this expression to our evaluation buffer.
    _input_line += part;
    _input_line += '\n';  // we stripped this off, above

    _result = "";
    _eval_done = true;
    _wait_done.notify_all();
}

std::string PythonEval::poll_result()
{
    if (not _eval_done)
    {
        // We don't have a real need to lock anything here; we're just
        // using this as a hack, so that the condition variable will
        // wake us up. The goal here is to block when there's no output
        // to be reported.
        auto evdone = [&](void) { return _eval_done; };
        std::unique_lock<std::mutex> lck(_poll_mtx);
        _wait_done.wait(lck, evdone);
    }

    std::string r = _result;
    _result.clear();
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
