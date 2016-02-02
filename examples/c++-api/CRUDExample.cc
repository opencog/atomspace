#include <iostream>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>

using namespace opencog;

/**
 * Sample code that demonstrates CRUD operations.
 */
int main(int argc, char ** args)
{
    AtomSpace as;

    // create atoms
    Handle h = as.add_node(CONCEPT_NODE, "Cat");
    Handle h1 = as.add_node(CONCEPT_NODE, "Human");
    Handle h2 = as.add_node(CONCEPT_NODE, "Animal");

    // Links can be ordered or unordered.InheritanceLink is a type of ordered link
    // Which means its element's ( outgoing sets) order is kept intact.
    HandleSeq hseq = { h1, h2 };
    Handle hinheritance = as.add_link(INHERITANCE_LINK, hseq);

    // Update atom's truth value
    SimpleTruthValuePtr tvinheritance(new SimpleTruthValue(0.5, 100));
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

    // Two ways to get outgoing set of a Link and incoming set of a node.
    HandleSeq outgoing = LinkCast(hinheritance)->getOutgoingSet();

    outgoing = LinkCast(h)->getOutgoingSet();

    opencog::IncomingSet incoming = NodeCast(h)->getIncomingSet();

    // Atoms who have incoming Links cannot be deleted before the links are deleted first.
    assert(not as.remove_atom(h));
    // Set the recursive delete flag to delete h including everything pointing to it.
    assert(as.remove_atom(h, true));

    // Creating hierarchical atomspace
    // http://wiki.opencog.org/wikihome/index.php/Multiple_AtomSpaces#Hierarchical_AtomSpaces
    AtomSpace child_as(&as);
    child_as.add_node(CONCEPT_NODE, "Cat"); //Not visible to parent atomsapce as.

    //Print outs of the content of the two atomspaces
    std::cout << "Main atomspace:" << std::endl << as << std::endl
              << "Child atomspace: " << std::endl << child_as << std::endl;

    return 0;
}
