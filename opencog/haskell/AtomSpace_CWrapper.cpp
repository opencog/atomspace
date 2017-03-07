
#include "AtomSpace_CWrapper.h"
#include "Utils_CWrapper.h"
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/FuzzyTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/util/exceptions.h>

AtomSpace* AtomSpace_new( AtomSpace* parent_ptr )
{
    return new AtomSpace(parent_ptr);
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
    Type t = classserver().getType(std::string(type));
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
    Type t = classserver().getType(std::string(type));
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
    *atom_out = this_ptr->add_link(t,oset);
    return 0;
}

int AtomSpace_getNode( AtomSpace* this_ptr
                     , const char* type
                     , const char* name
                     , Handle* atom_out)
{
    Type t = classserver().getType(std::string(type));
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
    Type t = classserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    HandleSeq oset;
    for(int i=0;i<size;i++)
        oset.push_back(*outgoing[i]);
    Handle h = this_ptr->get_link(t,oset);
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

    const std::string &str = classserver().getTypeName(h->getType());
    *type = (char*) malloc(sizeof(char) * (str.length()+1));
    if(! *type)
        throw RuntimeException(TRACE_INFO,"Failed malloc.");
    std::strcpy(*type, str.c_str());

    NodePtr ptr = NodeCast(h);
    if(ptr){ // It is a node.
        *node_or_link = 1;
        const std::string &str = ptr->getName();
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
        *out_len = lnk->getArity();

        *out = (Handle*)malloc(sizeof(Handle*) * (*out_len));

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

int AtomSpace_getTruthValue( AtomSpace* this_ptr
                           , Handle* atom
                           , Type* tv_type
                           , double* parameters )
{
    Handle h = *atom;
    if(h == Handle::UNDEFINED)
        return -1;
    TruthValuePtr tv = h->getTruthValue();
    Utils_toRawType(tv,tv_type,parameters);
    return 0;
}

int AtomSpace_setTruthValue( AtomSpace* this_ptr
                           , Handle* atom
                           , Type type
                           , double* parameters )
{
    Handle h = *atom;
    if (!h) // Invalid UUID parameter.
        return -1;

    if (type == SIMPLE_TRUTH_VALUE) {
        h->setTruthValue(SimpleTruthValue::createTV(parameters[0],parameters[1]));
    }
    else
    if (type == COUNT_TRUTH_VALUE) {
        h->setTruthValue(CountTruthValue::createTV(parameters[0]
                                                  ,parameters[2]
                                                  ,parameters[1]));
    }
    else
    if (type == INDEFINITE_TRUTH_VALUE) {
        IndefiniteTruthValuePtr iptr =
            IndefiniteTruthValue::createITV(parameters[1]
                                           ,parameters[2]
                                           ,parameters[3]);
        // iptr->setMean(parameters[0]);
        // iptr->setDiff(parameters[4]);
        h->setTruthValue(std::static_pointer_cast<TruthValue>(iptr));
    }
    else
    if (type == FUZZY_TRUTH_VALUE) {
        double count = FuzzyTruthValue::confidenceToCount(parameters[1]);
        h->setTruthValue(FuzzyTruthValue::createTV(parameters[0],count));
    }
    else
    if (type == PROBABILISTIC_TRUTH_VALUE) {
        h->setTruthValue(ProbabilisticTruthValue::createTV(parameters[0]
                                                          ,parameters[2]
                                                          ,parameters[1]));
    }
    else
        throw InvalidParamException(TRACE_INFO,
            "Invalid TruthValue Type parameter.");
    return 0;
}
