#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/executioncontext/Context.h>
#include <opencog/util/exceptions.h>

#include "TypeCtors.h"

using namespace opencog;

Handle opencog::add_node(Type t, std::string name)
{
    AtomSpacePtr atomspace = get_context_atomspace();
    if (atomspace == nullptr)
        throw RuntimeException(TRACE_INFO, "current atomspace is not set");

    return atomspace->add_node(t, std::move(name));
}

Handle opencog::add_link(Type t, HandleSeq outgoing)
{
    AtomSpacePtr atomspace = get_context_atomspace();
    if (atomspace == nullptr)
        throw RuntimeException(TRACE_INFO, "current atomspace is not set");

    return atomspace->add_link(t, std::move(outgoing));
}
