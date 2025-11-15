#include <opencog/util/Config.h>
#include <opencog/util/exceptions.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/cython/executioncontext/Context.h>
#include <iostream>
#include <stdexcept>

#include "Utilities.h"

using namespace opencog;

Handle opencog::add_node(Type t, std::string name) {
    AtomSpacePtr atomspace = get_context_atomspace();
    if (atomspace == nullptr){
        throw std::runtime_error("current atomspace is not set");
    }

    return atomspace->add_node(t, std::move(name));
}

Handle opencog::add_link(Type t, HandleSeq outgoing) {
    AtomSpacePtr atomspace = get_context_atomspace();
    if (atomspace == nullptr){
        throw std::runtime_error("current atomspace is not set");
    }

    return atomspace->add_link(t, std::move(outgoing));
}
