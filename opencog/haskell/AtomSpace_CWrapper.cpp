
#include "AtomSpace_CWrapper.h"
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/TruthValue.h>
#include <opencog/atomspace/SimpleTruthValue.h>
#include <opencog/atomspace/CountTruthValue.h>
#include <opencog/atomspace/IndefiniteTruthValue.h>
#include <opencog/atomspace/FuzzyTruthValue.h>
#include <opencog/atomspace/ProbabilisticTruthValue.h>

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

int AtomSpace_getTruthValue( AtomSpace* this_ptr
                           , long handle
                           , double *parameters )
{
    Handle h(handle);
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
                std::dynamic_pointer_cast<IndefiniteTruthValue>(tv);
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
            break;
    }
    return tv->getType();
}
