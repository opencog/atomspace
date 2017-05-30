/*
 * @file opencog/cython/PythonEval.h
 *
 * Simple python expression evaluator.
 *
 * @author Zhenhua Cai <czhedu@gmail.com>
 *         Ramin Barati <rekino@gmail.com>
 *         Keyvan Mir Mohammad Sadeghi <keyvan@opencog.org>
 *         Curtis Faith <curtis.m.faith@gmail.com>
 * @date   2011-09-20
 *
 * @Note
 *   Zhenhua: Many code are copied directly from original /opencog/cython/PythonModule.h|cc
 *            by Joel. I also borrowed some ideas from SchemeEval.h|cc
 *
 *   Ramin: This class is completely revised by me and Keyvan. The new code is inspired
 *          by Linas' SchemeEval and borrowed some ideas from Joel's PythonModule.
 *
 *  @todo
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

#ifndef OPENCOG_PYTHON_EVAL_H
#define OPENCOG_PYTHON_EVAL_H
#ifdef HAVE_CYTHON

#include "PyIncludeWrapper.h"

#include <condition_variable>
#include <map>
#include <mutex>
#include <string>
#include <vector>

#include <boost/filesystem/operations.hpp>

#include <opencog/atoms/base/Handle.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/eval/GenericEval.h>


namespace opencog {

class AtomSpace;

/**
 * Singleton class used to initialize python interpreter in the main thread.
 * It also provides some handy functions, such as getPyAtomspace. These helper
 * functions may need python GIL and you should do this manually.
 */
class PythonEval : public GenericEval
{
    private:
        void initialize_python_objects_and_imports(void);

        // Module utility functions
        void import_module( const boost::filesystem::path &file,
                            PyObject* pyFromList);
        void add_module_directory(const boost::filesystem::path &directory);
        void add_module_file(const boost::filesystem::path &file);
        void add_modules_from_path(std::string path);
        void add_modules_from_abspath(std::string path);

        // Python utility functions
        PyObject* call_user_function(const std::string& func,
                                     Handle varargs);
        void build_python_error_message(const char* function_name,
                                        std::string& errorMessage);
        void add_to_sys_path(std::string path);
        PyObject * atomspace_py_object(AtomSpace *);
        void print_dictionary(PyObject*);
        void execute_string(const char* command);
        int argument_count(PyObject* pyFunction);
        PyObject* module_for_function(const std::string& moduleFunction,
                                      std::string& functionName);

        static PythonEval* singletonInstance;

        AtomSpace* _atomspace;
        // Resource Acquisition is Allocation, for the current AtomSpace.
        // If anything throws an exception, then dtor runs and restores
        // the old atomspace.
        struct RAII {
            RAII(PythonEval* pev, AtomSpace* as) : _pev(pev)
               { _save_as = pev->_atomspace; pev->_atomspace = as; }
            ~RAII() {_pev->_atomspace = _save_as; }
            AtomSpace* _save_as;
            PythonEval* _pev;
        };

        // Single, global mutex for serializing access to the atomspace.
        // The singleton-instance design of this class forces us to
        // serialize access (the GIL is not enough), because there is
        // no way to guarantee that python won't accidentally be called
        // from multiple threads.  That's because the EvaluationLink
        // is called from scheme and from the pattern matcher, and its
        // unknown how many threads those things might be running in.
        // The lock is recursive, because we may need to use multiple
        // different atomspaces with the evaluator, in some nested
        // fashion. So this lock prevents other threads from using the
        // wrong atomspace in some other thread.  Quite unfortunate.
        static std::recursive_mutex _mtx;

        // Computed results are typically polled in a distinct thread.
        bool _eval_done;
        std::mutex _poll_mtx;
        std::mutex _eval_mutex;
        std::condition_variable _wait_done;

        PyObject* _pyGlobal;
        PyObject* _pyLocal;
        PyObject* _pyRootModule;

        PyObject* _pySysPath;

        std::map <std::string, PyObject*> _modules;

        std::string _result;
        int _paren_count;
        void eval_expr_line(const std::string&);

    public:
        PythonEval(AtomSpace*);
        ~PythonEval();

        /**
         * Create the singleton instance with the supplied atomspace.
         */
        static void create_singleton_instance(AtomSpace*);

        /**
         * Delete the singleton instance.
         */
        static void delete_singleton_instance();

        /**
         * Get a reference to the singleton instance.
         */
        static PythonEval & instance(AtomSpace* atomspace = NULL);

        // The async-output interface.
        virtual void begin_eval(void);
        virtual void eval_expr(const std::string&);
        virtual std::string poll_result(void);
        virtual void interrupt(void);

        // The synchronous-output interface.
        std::string eval(const std::string& expr)
        {
            std::lock_guard<std::mutex> lock(_eval_mutex);
            begin_eval(); eval_expr(expr); return poll_result();
        }

        /**
         * Runs the Python code contained in 'script'.
         */
        std::string apply_script(const std::string& script);

        /**
         * Calls the Python function passed in `func`, passing it
         * the `varargs` as an argument, and returning a Handle.
         */
        Handle apply(AtomSpace*, const std::string& func, Handle varargs);

        /**
         * Calls the Python function passed in `func`, passing it
         * the `varargs` as an argument, returning a TruthValuePtr.
         */
        TruthValuePtr apply_tv(AtomSpace*, const std::string& func, Handle varargs);

        /**
         * Calls the Python function passed in `func`, passing it
         * the AtomSpace as an argument, returning void.
         */
        void apply_as(const std::string& func, AtomSpace*);

        /**
         *
         */
        void print_root_dictionary()
            { this->print_dictionary(PyModule_GetDict(_pyRootModule)); }

};

/**
 * Initialize Python. Must be called before any Python dependent modules
 * are loaded.
 */ 
void global_python_initialize();

/**
 * Finalize Python. Call to cleanup memory used by Python interpreters.
 */
void global_python_finalize();

} /* namespace opencog */

#endif /* HAVE_CYTHON */
#endif /* OPENCOG_PYTHON_EVAL_H */
