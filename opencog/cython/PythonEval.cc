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

#include <dlfcn.h>
#include <unistd.h>
#include <chrono>
#include <filesystem>

#include <boost/scope_exit.hpp>

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/misc.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/executioncontext/Context.h>
#include "PythonEval.h"

// This is an header in the build dreictory, auto-gened by cython
#include "opencog/atomspace_api.h"

using namespace opencog;
using namespace std::chrono_literals;

#ifdef __APPLE__
  #define secure_getenv getenv
#endif

const int NO_SIGNAL_HANDLERS = 0;
const char* NO_FUNCTION_NAME = "";

/*
 * @todo When can we remove the singleton instance? Answer: not sure.
 * Python itself is single-threaded. That is, currently, python fakes
 * multi-thread support by grabbing a lock and only allowing just
 * one thread to run at a time.  Our singleton instance mirrors this
 * python limitation.  However, if we could set a per-thread atomspace,
 * then we could maybe still have python be callable in multiple threads,
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
PythonEval* PythonEval::singletonInstance = NULL;

PythonEval::PythonEval()
{
    // Check that this is the first and only PythonEval object.
    if (singletonInstance) {
        throw RuntimeException(TRACE_INFO,
            "Can't create more than one PythonEval singleton instance!");
    }

    _eval_done = true;
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
void PythonEval::create_singleton_instance()
{
    if (singletonInstance) return;

    // Create the single instance of a PythonEval object.
    singletonInstance = new PythonEval();
}

void PythonEval::delete_singleton_instance()
{
    if (!singletonInstance) return;

    // Delete the singleton PythonEval instance.
    delete singletonInstance;
    singletonInstance = NULL;
}

PythonEval& PythonEval::instance()
{
    // Make sure we have a singleton.
    if (!singletonInstance)
        create_singleton_instance();
    return *singletonInstance;
}

// ====================================================
// Initialization of search paths for python modules.

static const char* DEFAULT_PYTHON_MODULE_PATHS[] =
{
    NULL
};

static const char* PROJECT_PYTHON_MODULE_PATHS[] =
{
    PROJECT_BINARY_DIR"/opencog/cython", // bindings
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

    // If the current working directory is the projet build or source
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
 * that leads to success will then be declared the official system
 * path, henceforth. Woe unto those try to defy the will of the python
 * gods.
 *
 * Return true if the load worked, else return false.
 */
static bool try_to_load_modules(const char ** config_paths)
{
    PyObject* pySysPath = PySys_GetObject((char*)"path");

    Py_ssize_t pos_idx = 0;
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
            PyList_Insert(pySysPath, pos_idx, pyModulePath);
            Py_DECREF(pyModulePath);
            pos_idx += 1;
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
    // permutation of the config paths.  I'm confused, though, different
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
// Finding and loading python modules.

const int ABSOLUTE_IMPORTS_ONLY = 0;

void PythonEval::import_module(const std::filesystem::path &file,
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
void PythonEval::add_module_directory(const std::filesystem::path &directory)
{
    typedef std::vector<std::filesystem::path> PathList;
    PathList files;
    PathList pyFiles;

    // Loop over the files in the directory looking for Python files.
    copy(std::filesystem::directory_iterator(directory),
         std::filesystem::directory_iterator(), back_inserter(files));

    for (auto filepath: files)
    {
        if (filepath.extension() == std::filesystem::path(".py"))
            pyFiles.push_back(filepath);
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
    for (auto filepath: pyFiles)
        import_module(filepath, pyFromList);

    // Cleanup the reference count for the from list.
    Py_DECREF(pyFromList);
}

/**
* Add the .py file in the given path as a module to __main__ and add the
* reference to the dictionary (this->modules)
*/
void PythonEval::add_module_file(const std::filesystem::path &file)
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
        int stat_ret = stat(abspath.c_str(), &finfo);

        if (stat_ret != 0)
            return;

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
            std::string base = getcwd(NULL, 0);
            base += '/';
            base += pathString;
            auto pypath = std::filesystem::canonical(base);
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
    int stat_ret = stat(pathString.c_str(), &finfo);

    if (stat_ret != 0)
    {
        logger().warn() << "Python module path \'" << pathString
                        << "\' can't be found";
    }
    else
    {
        if (S_ISDIR(finfo.st_mode))
            add_module_directory(pathString);
        else if (S_ISREG(finfo.st_mode))
            add_module_file(pathString);
        else
            logger().warn() << "Python module path \'" << pathString
                            << "\' can't be found";
    }

    // Release the GIL. No Python API allowed beyond this point.
    PyGILState_Release(gstate);
}

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
        PyObject* pyModuleTmp = _modules[moduleName];

        // If not found, first check that it is not an object.
        // Then try loading it.
        // We have to guess, if its a single file, or an entire
        // directory with an __init__.py file in it ...
        if (nullptr == pyModuleTmp and
            nullptr == find_object(pyModule, moduleName))
        {
            add_modules_from_path(moduleName);
            add_modules_from_path(moduleName + ".py");
            pyModuleTmp = _modules[moduleName];
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

// ===========================================================
// Calling functions and applying functions to arguments
// Most of these are part of the public API.

std::recursive_mutex PythonEval::_mtx;

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
        std::string errorString =
            build_python_error_message(moduleFunction);
        PyErr_Clear();
        throw RuntimeException(TRACE_INFO, "%s", errorString.c_str());
    }
    return pyReturnValue;
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
    PyGILState_STATE gstate = PyGILState_Ensure();

    // BOOST_SCOPE_EXIT is a declaration for a scope-exit handler.
    // It will call PyGILState_Release() when this function returns
    // (e.g. due to a throw).  The below is not a call; it's just a
    // declaration. Anyway, once the GIL is released, no more python
    // API calls are allowed.
    BOOST_SCOPE_EXIT(&gstate) {
        PyGILState_Release(gstate);
    } BOOST_SCOPE_EXIT_END

    // Create the Python tuple for the function call with python
    // atoms for each of the atoms in the link arguments.
    size_t nargs = arguments->get_arity();
    PyObject* pyArguments = PyTuple_New(nargs);
    const HandleSeq& args = arguments->getOutgoingSet();
    for (size_t i=0; i<nargs; i++)
        PyTuple_SetItem(pyArguments, i, py_atom(args[i]));

    return do_call_user_function(moduleFunction, pyArguments);
}

/**
 * Apply the user function to the arguments passed in varargs and
 * return the extracted Value.
 */
ValuePtr PythonEval::apply_v(AtomSpace * as,
                             const std::string& func,
                             Handle varargs)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    push_context_atomspace(as);
    BOOST_SCOPE_EXIT(void) {
        pop_context_atomspace();
    } BOOST_SCOPE_EXIT_END

    // Get the python value object returned by this user function.
    PyObject *pyValue = call_user_function(func, varargs);

    // If we got a non-null Value there were no errors.
    if (NULL == pyValue)
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return Atomese!",
            func.c_str());

    // Grab the GIL.
    PyGILState_STATE gstate = PyGILState_Ensure();

    BOOST_SCOPE_EXIT(&gstate) {
        PyGILState_Release(gstate);
    } BOOST_SCOPE_EXIT_END

    // Did we actually get a Value?
    // One way to do this would be to say
    //    PyObject *vtype = find_object("Value");
    //    if (0 == PyObject_IsInstance(pyValue, vtype)) ...
    // but just grabbing the attr is easier, for now.
    if (0 == PyObject_HasAttrString(pyValue, "value_ptr"))
    {
        Py_DECREF(pyValue);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return Atomese!",
            func.c_str());
    }

    // Get the truth value pointer from the object (will be encoded
    // as a long by PyVoidPtr_asLong)
    PyObject *pyValuePtrPtr = PyObject_CallMethod(pyValue,
                                    (char*) "value_ptr", NULL);
    // Make sure we got a truth value pointer.
    PyObject *pyError = PyErr_Occurred();
    if (pyError or nullptr == pyValuePtrPtr)
    {
        Py_DECREF(pyValue);
        if (pyValuePtrPtr) Py_DECREF(pyValuePtrPtr);
        throw RuntimeException(TRACE_INFO,
            "Python function '%s' did not return Atomese!",
            func.c_str());
    }

    // Get the ValuePtr. Static cast, we were passed a ulong int.
    ValuePtr vptr(*(static_cast<ValuePtr*>
            (PyLong_AsVoidPtr(pyValuePtrPtr))));

    Py_DECREF(pyValuePtrPtr);
    Py_DECREF(pyValue);
    return vptr;
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

    // Grab the GIL.
    PyGILState_STATE gstate = PyGILState_Ensure();

    // BOOST_SCOPE_EXIT is a declaration for a scope-exit handler.
    // It will call PyGILState_Release() when this function returns
    // (e.g. due to a throw).  The below is not a call; it's just a
    // declaration. Anyway, once the GIL is released, no more python
    // API calls are allowed.
    BOOST_SCOPE_EXIT(&gstate) {
        PyGILState_Release(gstate);
    } BOOST_SCOPE_EXIT_END

    // Create the Python tuple for the function call with python
    // atomspace.
    PyObject* pyArguments = PyTuple_New(1);
    PyObject* pyAtomSpace = this->atomspace_py_object(as_argument);

    PyTuple_SetItem(pyArguments, 0, pyAtomSpace);
    // Py_DECREF(pyAtomSpace);

    // Execute the user function.
    do_call_user_function(moduleFunction, pyArguments);
}

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
#if PY_MAJOR_VERSION == 2
    char* pythonErrorString = PyBytes_AsString(pyErrorString);
#else
    const char* pythonErrorString = PyUnicode_AsUTF8(pyErrorString);
#endif
    if (pythonErrorString) {
        errorStringStream << ": " << pythonErrorString << ".";
    } else {
        errorStringStream << ": Undescribed Error.";
    }

    // Print the traceback, too, if it is provided.
    if (pyTraceback)
    {
#if PY_MAJOR_VERSION == 2
        PyObject* pyTBString = PyObject_Str(pyTraceback);
        char* tb = PyBytes_AsString(pyTBString);
        errorStringStream << "\nTraceback: " << tb;
        Py_DECREF(pyTBString);
#else
        errorStringStream << "\nTraceback (most recent call last):\n";

        PyTracebackObject* pyTracebackObject = (PyTracebackObject*)pyTraceback;

        while (pyTracebackObject != NULL)
        {
            int line_number = pyTracebackObject-> tb_lineno;
            const char* filename = PyUnicode_AsUTF8(
                    pyTracebackObject->tb_frame->f_code->co_filename);
            const char* code_name = PyUnicode_AsUTF8(
                    pyTracebackObject->tb_frame->f_code->co_name);

            errorStringStream << "File \"" << filename <<"\", ";
            errorStringStream << "line " << line_number <<", ";
            errorStringStream << "in " << code_name <<"\n";

            pyTracebackObject = pyTracebackObject -> tb_next;
        }
#endif
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
#if PY_MAJOR_VERSION < 3
        retval = PyString_AsString(obrep);
#else
        PyObject* pyStr = nullptr;
        if (not PyBytes_Check(obrep))
        {
            pyStr = PyUnicode_AsEncodedString(obrep, "UTF-8", "strict");
            Py_DECREF(obrep);
            obrep = pyStr;
        }
        retval = PyBytes_AS_STRING(obrep);
#endif
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
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    // Grab the GIL
    PyGILState_STATE gstate = PyGILState_Ensure();

    // BOOST_SCOPE_EXIT is a declaration for a scope-exit handler.
    // It will call PyGILState_Release() when this function returns
    // (e.g. due to a throw).  The below is not a call; it's just a
    // declaration. Anyway, once the GIL is released, no more python
    // API calls are allowed.
    BOOST_SCOPE_EXIT(&gstate) {
        PyGILState_Release(gstate);
    } BOOST_SCOPE_EXIT_END

    check_for_error();

    // Execute the script. NOTE: This call replaces PyRun_SimpleString
    // which was masking errors because it calls PyErr_Clear() so the
    // call to PyErr_Occurred below was returning false even when there
    // was an error.
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

    BOOST_SCOPE_EXIT(&pipefd, &rc, &stdout_backup, &_capture_stdout)
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
    } BOOST_SCOPE_EXIT_END

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
   return &opencog::PythonEval::instance();
}

};

// =========== END OF FILE =========
