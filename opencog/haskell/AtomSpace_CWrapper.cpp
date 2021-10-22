#include <cstring>

#include "AtomSpace_CWrapper.h"
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/util/exceptions.h>

AtomSpace* AtomSpace_new( AtomSpace* parent_ptr )
{
    return createAtomSpace(parent_ptr).get();
}

void AtomSpace_delete( AtomSpace* this_ptr )
{
    delete this_ptr;
}

int AtomSpace_addNode( AtomSpace* this_ptr
                     , const char* type
                     , const char* name
                     , Handle* atom_out)
{
    Type t = nameserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    *atom_out = this_ptr->add_node(t,std::string(name));
    return 0;
}

int AtomSpace_addLink( AtomSpace* this_ptr
                     , const char* type
                     , const Handle** outgoing
                     , int size
                     , Handle* atom_out)
{
    Type t = nameserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);

    HandleSeq oset;
    for(int i=0;i<size;i++) {
        oset.push_back(*outgoing[i]);
    }
    for(int i=0;i<size;i++) {
        if(!oset[i]) // Atom doesn't exist.
            return -1;
    }
    *atom_out = this_ptr->add_link(t, std::move(oset));
    return 0;
}

int AtomSpace_getNode( AtomSpace* this_ptr
                     , const char* type
                     , const char* name
                     , Handle* atom_out)
{
    Type t = nameserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    Handle h = this_ptr->get_node(t,std::string(name));
    *atom_out = h;

    return h == Handle::UNDEFINED;
}

int AtomSpace_getLink( AtomSpace* this_ptr
                     , const char* type
                     , const Handle** outgoing
                     , int size
                     , Handle* atom_out)
{
    Type t = nameserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    HandleSeq oset;
    for(int i=0;i<size;i++)
        oset.push_back(*outgoing[i]);
    Handle h = this_ptr->get_link(t, std::move(oset));
    *atom_out = h;

    return h == Handle::UNDEFINED;
}

int AtomSpace_removeAtom( AtomSpace* this_ptr
                        , Handle* atom)
{
    if(this_ptr->remove_atom(*atom))
        return 0;
    return -1;
}

int AtomSpace_getAtomByHandle( AtomSpace* this_ptr
                             , Handle* atom
                             , int* node_or_link
                             , char** type
                             , char** name
                             , Handle** out
                             , int* out_len)
{
    Handle h = *atom;
    if(!h) // Invalid UUID parameter.
        return -1;

    const std::string &str = nameserver().getTypeName(h->get_type());
    *type = (char*) malloc(sizeof(char) * (str.length()+1));
    if(! *type)
        throw RuntimeException(TRACE_INFO,"Failed malloc.");
    std::strcpy(*type, str.c_str());

    NodePtr ptr = NodeCast(h);
    if(ptr){ // It is a node.
        *node_or_link = 1;
        const std::string &str = ptr->get_name();
        *name = (char*) malloc(sizeof(char) * (str.length()+1));
        if(! *name)
            throw RuntimeException(TRACE_INFO,"Failed malloc.");
        std::strcpy(*name, str.c_str());
        return 0;
    }else{ // It is a link.
        *node_or_link = 0;
        LinkPtr lnk = LinkCast(h);
        if(!lnk)
            throw RuntimeException(TRACE_INFO,"Error in cast Link.");
        *out_len = lnk->get_arity();

        *out = (Handle*)malloc(sizeof(Handle) * (*out_len));

        if(! *out)
            throw RuntimeException(TRACE_INFO,"Failed malloc.");
        for(int i=0;i<(*out_len);i++)
            (*out)[i] = lnk->getOutgoingAtom(i);
        return 0;
    }
}

void AtomSpace_debug( AtomSpace* this_ptr )
{
    std::cerr<<(*this_ptr);
}
