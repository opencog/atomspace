/**
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
 *   Zhenhua: Many code are copied directly from original
 *            /opencog/cython/PythonModule.h|cc
 *            by Joel. I also borrowed some ideas from SchemeEval.h|cc
 *
 *   Ramin: This class is completely revised by me and Keyvan. The new
 *          code is inspired by Linas' SchemeEval and borrowed some
 *          ideas from Joel's PythonModule.
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

#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/eval/GenericEval.h>

namespace opencog {

/**
 * Singleton class used to initialize python interpreter in the main thread.
 * It also provides some handy functions, such as getPyAtomspace. These helper
 * functions may need python GIL and you should do this manually.
 */
class PythonEval : public GenericEval
{
    private:
        void initialize_python_objects_and_imports(void);

        // Python utility functions
        PyObject* atomspace_py_object(AtomSpacePtr);
        PyObject* get_function(const std::string& moduleFunction);
        PyObject* do_call_user_function(const std::string& moduleFunction,
                                        PyObject* pyArguments);

        // Call functions; execute scripts.
        ValuePtr call_user_function(const std::string& func,
                                    const HandleSeq& args);
        std::string build_python_error_message(const std::string&);
        void throw_python_exception(const std::string&);

        std::string execute_string(const char*);
        std::string execute_script(const std::string&);
        std::string exec_wrap_stdout(const std::string&);

        // Computed results are typically polled in a distinct thread.
        bool _eval_done;
        std::mutex _poll_mtx;
        std::mutex _eval_mutex;
        std::condition_variable _wait_done;

        PyObject* _pyRootModule;

        std::string _result;
        std::string _capture_stdout;
        int _paren_count;
        void eval_expr_line(const std::string&);
        bool check_for_error();

    protected:
        AtomSpacePtr _atomspace;

    public:
        PythonEval(void);
        ~PythonEval();
        virtual std::string get_name(void) const { return "PythonEval"; }

        // Return per-thread, per-atomspace evaluator using pool management
        static PythonEval* get_python_evaluator(AtomSpace*);
        static PythonEval* get_python_evaluator(const AtomSpacePtr&);

        virtual void set_atomspace(const AtomSpacePtr&);
        virtual AtomSpacePtr get_atomspace(void);

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
         * Calls the Python function passed in `func`, passing it
         * the `varargs` as an argument, and returning a Handle.
         */
        virtual ValuePtr apply_v(AtomSpace * as, const std::string& func,
                         Handle varargs);

        /**
         * Calls the Python function passed in `func`, passing it
         * the `varargs` as an argument, and returning a Handle.
         */
        Handle apply(AtomSpace * as, const std::string& func,
                     Handle varargs)
        { return HandleCast(apply_v(as, func, varargs)); }

        /**
         * Calls the Python function passed in `func`, passing it
         * the AtomSpace as an argument, returning void.
         */
        void apply_as(const std::string& func, AtomSpace*);
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

extern "C" {
   // For shared-library loading
   opencog::PythonEval* get_python_evaluator(opencog::AtomSpace*);
};

} /* namespace opencog */

#endif /* HAVE_CYTHON */
#endif /* OPENCOG_PYTHON_EVAL_H */
