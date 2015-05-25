
#include <opencog/atomspace/AtomSpace.h>

extern "C"
{
    using namespace opencog;

    AtomSpace* AtomSpace_new();
    void AtomSpace_delete(AtomSpace* this_ptr);
    long AtomSpace_addNode(AtomSpace* this_ptr, Type t, const char* name);
    void AtomSpace_print(AtomSpace* this_ptr);
/*
    int AtomSpace_getSize(AtomSpace* this_ptr);
    int AtomSpace_getNumNodes(AtomSpace* this_ptr);
    int AtomSpace_getNumLinks(AtomSpace* this_ptr);
    void AtomSpace_addAtom(AtomSpace* this_ptr, AtomPtr atom);
    Handle AtomSpace_addLink(AtomSpace* this_ptr, Type t, const HandleSeq& outgoing);
    bool AtomSpace_removeAtom(AtomSpace* this_ptr, Handle h);
*/
}

