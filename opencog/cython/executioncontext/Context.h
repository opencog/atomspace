#include <opencog/atomspace/AtomSpace.h>
#include <deque>

#ifndef _OPENCOG_CONTEXT_H
#define _OPENCOG_CONTEXT_H

namespace opencog {
    extern thread_local std::deque<AtomSpacePtr> current;

    AtomSpacePtr get_context_atomspace();
    void push_context_atomspace(AtomSpacePtr atomspace);
    AtomSpacePtr pop_context_atomspace();
    void clear_context();
}

#endif // _OPENCOG_CONTEXT_H
