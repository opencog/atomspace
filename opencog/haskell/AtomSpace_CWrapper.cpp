
#include "AtomSpace_CWrapper.h"
#include <opencog/atomspace/ClassServer.h>

AtomSpace* AtomSpace_new()
{
    return new AtomSpace();
}

void AtomSpace_delete( AtomSpace* this_ptr )
{
    delete this_ptr;
}

long AtomSpace_addNode( AtomSpace* this_ptr
                      , const char* type
                      , const char* name )
{
    Type t = classserver().getType(std::string(type));
    return this_ptr->addNode(t,std::string(name)).value();
}

long AtomSpace_addLink( AtomSpace* this_ptr
                      , const char* type
                      , const long* outgoing
                      , int size )
{
    Type t = classserver().getType(std::string(type));
    HandleSeq oset;
    for(int i=0;i<size;i++)
        oset.push_back(Handle(outgoing[i]));
    return this_ptr->addLink(t,oset).value();
}

long AtomSpace_getNode( AtomSpace* this_ptr
                      , const char* type
                      , const char* name
                      , int* found )
{
    Type t = classserver().getType(std::string(type));
    Handle h = this_ptr->getNode(t,std::string(name));
    *found = h != Handle::UNDEFINED;
    return h.value();
}

long AtomSpace_getLink( AtomSpace* this_ptr
                      , const char* type
                      , const long* outgoing
                      , int size
                      , int* found )
{
    Type t = classserver().getType(std::string(type));
    HandleSeq oset;
    for(int i=0;i<size;i++)
        oset.push_back(Handle(outgoing[i]));
    Handle h = this_ptr->getLink(t,oset);
    *found = h != Handle::UNDEFINED;
    return h.value();
}

int AtomSpace_removeAtom( AtomSpace* this_ptr
                        , long handle )
{
    return this_ptr->removeAtom(Handle(handle));
}

void AtomSpace_debug( AtomSpace* this_ptr )
{
    std::cerr<<(*this_ptr);
}

