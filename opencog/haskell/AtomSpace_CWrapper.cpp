
#include "AtomSpace_CWrapper.h"
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/TruthValue.h>
#include <opencog/atomspace/SimpleTruthValue.h>
#include <opencog/atomspace/CountTruthValue.h>
#include <opencog/atomspace/IndefiniteTruthValue.h>
#include <opencog/atomspace/FuzzyTruthValue.h>
#include <opencog/atomspace/ProbabilisticTruthValue.h>
#include <opencog/util/exceptions.h>

AtomSpace* AtomSpace_new( AtomSpace* parent_ptr )
{
    return new AtomSpace(parent_ptr);
}

void AtomSpace_delete( AtomSpace* this_ptr )
{
    delete this_ptr;
}

UUID AtomSpace_addNode( AtomSpace* this_ptr
                      , const char* type
                      , const char* name )
{
    Type t = classserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    return this_ptr->add_node(t,std::string(name)).value();
}

UUID AtomSpace_addLink( AtomSpace* this_ptr
                      , const char* type
                      , const UUID* outgoing
                      , int size )
{
    Type t = classserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    HandleSeq oset;
    for(int i=0;i<size;i++)
        oset.push_back(Handle(outgoing[i]));
    return this_ptr->add_link(t,oset).value();
}

UUID AtomSpace_getNode( AtomSpace* this_ptr
                      , const char* type
                      , const char* name
                      , int* found )
{
    Type t = classserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    Handle h = this_ptr->get_node(t,std::string(name));
    *found = h != Handle::UNDEFINED;
    return h.value();
}

UUID AtomSpace_getLink( AtomSpace* this_ptr
                      , const char* type
                      , const UUID* outgoing
                      , int size
                      , int* found )
{
    Type t = classserver().getType(std::string(type));
    if(t == NOTYPE)
        throw InvalidParamException(TRACE_INFO,
            "Invalid AtomType parameter '%s'.",type);
    HandleSeq oset;
    for(int i=0;i<size;i++)
        oset.push_back(Handle(outgoing[i]));
    Handle h = this_ptr->get_link(t,oset);
    *found = h != Handle::UNDEFINED;
    return h.value();
}

int AtomSpace_removeAtom( AtomSpace* this_ptr
                        , UUID handle )
{
    return this_ptr->remove_atom(Handle(handle));
}

int AtomSpace_getAtomByHandle( AtomSpace* this_ptr
                             , UUID handle
                             , char** type
                             , char** name
                             , UUID** out
                             , int* out_len)
{
    Handle h(handle);
    if(!h)
        throw InvalidParamException(TRACE_INFO,
            "Invalid Handler parameter.");

    const std::string &str = classserver().getTypeName(h->getType());
    (*type) = (char*) malloc(sizeof(char) * (str.length()+1));
    if(!(*type))
        throw RuntimeException(TRACE_INFO,"Failed malloc.");
    std::strcpy(*type, str.c_str());

    NodePtr ptr = NodeCast(h);
    if(ptr){//It is a node.
        const std::string &str = ptr->getName();
        (*name) = (char*) malloc(sizeof(char) * (str.length()+1));
        if(!(*name))
            throw RuntimeException(TRACE_INFO,"Failed malloc.");
        std::strcpy(*name, str.c_str());
        return 1;
    }else{//It is a link.
        LinkPtr lnk = LinkCast(h);
        if(!lnk)
            throw RuntimeException(TRACE_INFO,"Error in cast Link.");
        *out_len=lnk->getArity();
        (*out) = (UUID*) malloc(sizeof(UUID) * (*out_len));
        if(!(*out))
            throw RuntimeException(TRACE_INFO,"Failed malloc.");
        int i;
        for(i=0;i<(*out_len);i++)
            (*out)[i]=lnk->getOutgoingAtom(i).value();
        return 0;
    }
}

void AtomSpace_debug( AtomSpace* this_ptr )
{
    std::cerr<<(*this_ptr);
}

TruthValueType AtomSpace_getTruthValue( AtomSpace* this_ptr
                                      , UUID handle
                                      , double* parameters )
{
    Handle h(handle);
    if(!h)
        throw InvalidParamException(TRACE_INFO,
            "Invalid Handler parameter.");
    TruthValuePtr tv = h->getTruthValue();
    switch(tv->getType())
    {
        case SIMPLE_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getConfidence();
            break; }
        case COUNT_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getCount();
            parameters[2]=tv->getConfidence();
            break; }
        case INDEFINITE_TRUTH_VALUE: {
            IndefiniteTruthValuePtr itv =
                std::static_pointer_cast<IndefiniteTruthValue>(tv);
            parameters[0]=itv->getMean();
            parameters[1]=itv->getL();
            parameters[2]=itv->getU();
            parameters[3]=itv->getConfidenceLevel();
            parameters[4]=itv->getDiff();
            break; }
        case FUZZY_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getConfidence();
            break; }
        case PROBABILISTIC_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getCount();
            parameters[2]=tv->getConfidence();
            break; }
        default:
            throw RuntimeException(TRACE_INFO,
                "Invalid TruthValue Type.");
            break;
    }
    return tv->getType();
}

void AtomSpace_setTruthValue( AtomSpace* this_ptr
                            , UUID handle
                            , TruthValueType type
                            , double* parameters )
{
    Handle h(handle);
    if(!h)
        throw InvalidParamException(TRACE_INFO,
            "Invalid Handler parameter.");
    switch(type)
    {
        case SIMPLE_TRUTH_VALUE: {
            double count = SimpleTruthValue::confidenceToCount(parameters[1]);
            h->setTruthValue(SimpleTruthValue::createTV(parameters[0],count));
            break; }
        case COUNT_TRUTH_VALUE: {
            h->setTruthValue(CountTruthValue::createTV(parameters[0]
                                                      ,parameters[2]
                                                      ,parameters[1]));
            break; }
        case INDEFINITE_TRUTH_VALUE: {
            IndefiniteTruthValuePtr iptr =
                IndefiniteTruthValue::createITV(parameters[1]
                                               ,parameters[2]
                                               ,parameters[3]);
            iptr->setMean(parameters[0]);
            iptr->setDiff(parameters[4]);
            h->setTruthValue(std::static_pointer_cast<TruthValue>(iptr));
            break; }
        case FUZZY_TRUTH_VALUE: {
            double count = FuzzyTruthValue::confidenceToCount(parameters[1]);
            h->setTruthValue(FuzzyTruthValue::createTV(parameters[0],count));
            break; }
        case PROBABILISTIC_TRUTH_VALUE: {
            h->setTruthValue(ProbabilisticTruthValue::createTV(parameters[0]
                                                              ,parameters[2]
                                                              ,parameters[1]));
            break; }
        default:
            throw InvalidParamException(TRACE_INFO,
                "Invalid TruthValue Type parameter.");
            break;
    }
}
