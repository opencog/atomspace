#include <iostream>

#include <boost/signals2.hpp>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/AttentionValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/atomspace/types.h>

using namespace opencog;

/**
 * Sample code that shows how to listen for AtomSpace related events.
 * The followings are all the events/signals supported by the AtomSpace.
 */

void AtomAddedCBHandler(const Handle& h);
void AVChangedCBHandler(const Handle& h, const AttentionValuePtr& av_old,
                        const AttentionValuePtr& av_new);
void TVChangedCBHandler(const Handle& h, const TruthValuePtr& av_old,
                        const TruthValuePtr& tv_new);
void AtomRemovedCBHandler(const AtomPtr&);
void AtomAddedToAFCBHandler(const Handle& h, const AttentionValuePtr& av_old,
                            const AttentionValuePtr& av_new);
void AtomRemovedFromAFCBHandler(const Handle& h,
                                const AttentionValuePtr& av_old,
                                const AttentionValuePtr& av_new);

int main(int argc, char ** args)
{
    AtomSpace as;

    boost::signals2::connection AtomAddedSignalConn,
                                AVChangedSignalConn,
                                TVChangedSignalConn,
                                AtomsRemovedSignalConn,
                                AtomAddedToAttentionalFocusSignalConn,
                                AtomRemovedFromAttentionalFocusSignalConn;

    //Register Atomspace Event call back handlers.
    AtomAddedSignalConn = as.addAtomSignal(
            boost::bind(&AtomAddedCBHandler, _1));
    AVChangedSignalConn = as.AVChangedSignal(
            boost::bind(&AVChangedCBHandler, _1, _2, _3));
    TVChangedSignalConn = as.TVChangedSignal(
            boost::bind(&TVChangedCBHandler, _1, _2, _3));
    AtomsRemovedSignalConn = as.removeAtomSignal(
            boost::bind(&AtomRemovedCBHandler, _1));
    AtomAddedToAttentionalFocusSignalConn = as.AddAFSignal(
            boost::bind(&AtomAddedToAFCBHandler, _1, _2, _3));
    AtomRemovedFromAttentionalFocusSignalConn = as.RemoveAFSignal(
            boost::bind(&AtomRemovedFromAFCBHandler, _1, _2, _3));

    // create atoms
    Handle h = as.add_node(CONCEPT_NODE, "Cat");
    Handle h1 = as.add_node(CONCEPT_NODE, "Human");
    Handle h2 = as.add_node(CONCEPT_NODE, "Animal");

    // Links can be ordered or unordered.InheritanceLink is a type of ordered link
    // Which means its element's ( outgoing sets) order is kept intact.
    HandleSeq hseq = { h1, h2 };
    Handle hinheritance = as.add_link(INHERITANCE_LINK, hseq);

    // Update atom's truth value
    SimpleTruthValuePtr tvinheritance = std::make_shared<SimpleTruthValue>(0.5, 100);
    hinheritance->setTruthValue(tvinheritance);

    // Update atom's Attention value. Attention values are managed by the
    // ECAN(http://wiki.opencog.org/wikihome/index.php/Attention_Allocation)
    //mind agents.Not meant to be used by other modules.
    hinheritance->setAttentionValue(
            AttentionValuePtr(new AttentionValue(15, 30, 45)));

    // ListLink is an example of unordered type of Link.
    // Thus order is not guaranteed in this case.
    hseq = {h, h2};
    Handle hllink = as.add_link(LIST_LINK, hseq);

    // Set the recursive delete flag to delete h including everything pointing to it.
    assert(as.remove_atom(h, true));

    return 0;
}


void AtomAddedCBHandler(const Handle& h)
{

    std::cout << "An atom added.\n" << h->toShortString() << std::endl;
}

void AVChangedCBHandler(const Handle& h, const AttentionValuePtr& av_old,
                        const AttentionValuePtr& av_new)
{

    std::cout << "An atom's attention value changed.\n" << h->toShortString()
              << std::endl;
}

void TVChangedCBHandler(const Handle& h, const TruthValuePtr& av_old,
                        const TruthValuePtr& tv_new)
{
    std::cout << "An atom's truth value changed.\n" << h->toShortString()
              << std::endl;
}

void AtomRemovedCBHandler(const AtomPtr& a)
{
    std::cout << "An atom is removed.\n" << std::endl;
}

void AtomAddedToAFCBHandler(const Handle& h, const AttentionValuePtr& av_old,
                            const AttentionValuePtr& av_new)
{
    std::cout << "An atom was added to Attentional Focus.\n"
              << h->toShortString() << std::endl;
}

void AtomRemovedFromAFCBHandler(const Handle& h,
                                const AttentionValuePtr& av_old,
                                const AttentionValuePtr& av_new)
{
    std::cout << "An atom was removed from Attentional Focus.\n"
              << h->toShortString() << std::endl;
}
