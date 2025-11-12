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

// This is an header in the build dreictory, auto-gened by cython
#include "opencog/atomspace_api.h"

#include <dlfcn.h>

#ifdef __APPLE__
  #define secure_getenv getenv
#endif

using namespace opencog;

const int NO_SIGNAL_HANDLERS = 0;

// ====================================================
// Initialization of search paths for python modules.

/**
 * Get Python module search paths from the PYTHONPATH environment variable.
 * This replaces the old hardcoded PROJECT_PYTHON_MODULE_PATHS approach.
 *
 * CMake test infrastructure sets PYTHONPATH appropriately for tests,
 * and installed versions will use the system PYTHONPATH.
 */
static const char** get_module_paths()
{
    static const char** paths = nullptr;

    if (paths) return paths;

    // Count paths in PYTHONPATH
    unsigned nenv = 0;
    char* pypath = secure_getenv("PYTHONPATH");
    if (pypath) {
        char *p = pypath;
        while (p) { p = strchr(p+1, ':'); nenv++; }
    }

    // Allocate array for paths (+1 for NULL terminator)
    paths = (const char**) malloc(sizeof(char *) * (nenv + 1));

    // Parse PYTHONPATH and populate array
    int ip = 0;
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
            PyObject* pyModulePath = PyUnicode_DecodeUTF8(
                  config_paths[i], strlen(config_paths[i]), "strict");
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
// Python 3.9 came out in October 2020
#if PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION < 9
        PyEval_InitThreads();
#endif

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

    // Add the module name to the root module.
    // PyModule_AddObject steals the reference returned by PyImport_ImportModuleLevel.
    // The module remains accessible via sys.modules.
    PyModule_AddObject(_pyRootModule, moduleName.c_str(), pyModule);
}

/**
* Add all the .py files in the given directory as modules to __main__
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

        // Check Python's sys.modules directly (no need for separate cache)
        PyObject* sys_modules = PyImport_GetModuleDict();
        PyObject* pyModuleTmp = nullptr;

        if (sys_modules)
        {
            pyModuleTmp = PyDict_GetItemString(sys_modules, moduleName.c_str());
        }

        // If not found, first check that it is not an object.
        // Then try loading it.
        // We have to guess, if its a single file, or an entire
        // directory with an __init__.py file in it ...
        if (nullptr == pyModuleTmp and
            nullptr == find_object(pyModule, moduleName))
        {
            add_modules_from_path(moduleName);
            add_modules_from_path(moduleName + ".py");

            // Check sys.modules again after attempted import
            if (sys_modules)
            {
                pyModuleTmp = PyDict_GetItemString(sys_modules, moduleName.c_str());
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

// =========== END OF FILE =========
