
#include "AtomSpace_CWrapper.h"

AtomSpace* AtomSpace_new()
{
    return new AtomSpace();
}

void AtomSpace_delete(AtomSpace* this_ptr)
{
    delete this_ptr;
}

void AtomSpace_addNode(AtomSpace* this_ptr, Type t, const char* name)
{
    this_ptr->addNode(t,std::string(name));
}

void AtomSpace_print(AtomSpace* this_ptr)
{
    this_ptr->print();
}
/*
int AtomSpace_getSize(AtomSpace* this_ptr)
{
    this_ptr->getSize();
}

int AtomSpace_getNumNodes(AtomSpace* this_ptr)
{
    this_ptr->getNumNodes();
}

int AtomSpace_getNumLinks(AtomSpace* this_ptr)
{
    this_ptr->getNumLinks();
}

void AtomSpace_addAtom(AtomSpace* this_ptr, AtomPtr atom)
{
    this_ptr->addAtom(atom);
}
bool AtomSpace_removeAtom(AtomSpace* this_ptr, Handle h)
{
    this_ptr->removeAtom(h);
}
*/
