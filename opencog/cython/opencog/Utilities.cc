#include <opencog/util/Config.h>
#include <opencog/util/exceptions.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/cython/executioncontext/Context.h>
#include <iostream>
#include <stdexcept>

#include "Utilities.h"

using namespace opencog;

void opencog::initialize_python()
{
    // Initialize Python.
    logger().debug("initialize_opencog - initializing Python");
    global_python_initialize();

    // Tell the python evaluator to create its singleton instance
    // with our atomspace.
    logger().debug("initialize_opencog - creating PythonEval singleton instance");
    PythonEval::create_singleton_instance();
}

void opencog::finalize_python()
{
    // Delete the singleton instance of the PythonEval.
    PythonEval::delete_singleton_instance();

    // Cleanup Python.
    global_python_finalize();
}

Handle opencog::add_node(Type t, std::string name) {
    AtomSpace * atomspace = get_context_atomspace();
    if (atomspace == nullptr){
        throw std::runtime_error("current atomspace is not set");
    }

    return atomspace->add_node(t, std::move(name));
}

Handle opencog::add_link(Type t, HandleSeq outgoing) {
    AtomSpace * atomspace = get_context_atomspace();
    if (atomspace == nullptr){
        throw std::runtime_error("current atomspace is not set");
    }

    return atomspace->add_link(t, std::move(outgoing));
}
