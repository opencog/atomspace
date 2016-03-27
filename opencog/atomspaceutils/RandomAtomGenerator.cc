
/** RandomAtomGenerator.cc */

#include <opencog/util/random.h>

#include <opencog/atoms/base/types.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/atomspace/TLB.h>
#include <opencog/truthvalue/TruthValue.h>

#include "RandomAtomGenerator.h"

namespace opencog {

RandomAtomGenerator::RandomAtomGenerator(AtomSpace* atomspace,
                                         unsigned long random_seed,
                                         float link_size_mean,
                                         Type default_node_type,
                                         float chance_of_non_default_node,
                                         Type default_link_type,
                                         float chance_of_non_default_link,
                                         float chance_of_default_tv)
{
    _atomspace = atomspace;

    _default_node_type = default_node_type;
    _chance_of_non_default_node = chance_of_non_default_node;
    _default_link_type = default_link_type;
    _chance_of_non_default_link = chance_of_non_default_link;
    _link_size_mean = link_size_mean;

    _total_types = classserver().getNumberOfClasses();

    _counter = 0;
    _chance_of_default_tv = chance_of_default_tv;

    if (random_seed != USE_TIME_RANDOM_SEED)
        _random_seed = random_seed;
    else
        _random_seed = (unsigned long) time(NULL);

    // Create the random generator with the correct seed value.
    _random_generator = new MT19937RandGen(_random_seed);

    // Create the poisson distribution around the link mean.
    _poisson_distribution = new std::poisson_distribution<unsigned>(_link_size_mean);

    // Set the UUID for the first node.
    _first_out_UUID = TLB::getMaxUUID();
    _last_out_UUID = TLB::getMaxUUID();
}

RandomAtomGenerator::~RandomAtomGenerator()
{
    delete _poisson_distribution;
    delete _random_generator;
}


Type RandomAtomGenerator::random_type(Type parent_type)
{
    OC_ASSERT(parent_type < _total_types);
    Type candidate_type;

    // Loop until we get a type that is a subclass of t, skipping TYPE_NODE
    // since that type can't handle randomly generated names. Also skip other
    // validated types since the validation will fail.
    do {
        candidate_type = ATOM + _random_generator->randint(_total_types - ATOM - 1);
    } while (!classserver().isA(candidate_type, parent_type) or
        classserver().isA(candidate_type, FREE_LINK) or
        classserver().isA(candidate_type, SCOPE_LINK) or
        candidate_type == VARIABLE_LIST or
        candidate_type == DEFINE_LINK or
        candidate_type == NUMBER_NODE or
        candidate_type == TYPE_NODE);

    return candidate_type;
}

void RandomAtomGenerator::set_truth_value(Handle& atom)
{
    // Set the truth value to a non default using the chance threshold.
    if (_random_generator->randfloat() > _chance_of_default_tv) {
        float strength = _random_generator->randfloat();
        float confidence = _random_generator->randfloat();
        TruthValuePtr stv = SimpleTruthValue::createTV(strength, confidence);
        atom->setTruthValue(stv);
    }
}

Type RandomAtomGenerator::get_node_type()
{
    Type node_type;
    if (_random_generator->randfloat() < _chance_of_non_default_node)
        node_type = random_type(NODE);
    else
        node_type = _default_node_type;
    return node_type;
}

void RandomAtomGenerator::make_random_node()
{
    // Get the node type (non-default based on random chance and threshold).
    Type node_type = get_node_type();

    // Generate the node name.
    _counter++;
    std::string node_name("node "); 
    node_name += std::to_string(_counter);

    // Add the node to the atomspace.
    Handle node = _atomspace->add_node(node_type, node_name);

    // Set the truth value (non-default based on random chance and threshold).
    set_truth_value(node);
}

Type RandomAtomGenerator::get_link_type()
{
    Type link_type;
    if (_random_generator->randfloat() < _chance_of_non_default_link)
        link_type = random_type(LINK);
    else
        link_type = _default_link_type;
    return link_type;
}

Handle RandomAtomGenerator::get_random_handle()
{
    int range = _last_out_UUID - _first_out_UUID - 1;
    UUID random_UUID = _first_out_UUID + _random_generator->randint(range);
    return Handle(random_UUID);
}

bool RandomAtomGenerator::sequence_contains(HandleSeq& sequence, Handle& target)
{
    if (std::find(sequence.begin(), sequence.end(), target) != sequence.end())
        return true;
    else
        return false;
}

void RandomAtomGenerator::make_random_link()
{
    Handle link = Handle::UNDEFINED;

    // Loop until we add a link in case we randomly create a duplicate which
    // will not create a new link and throw off our counts.
    int initial_atom_count = _atomspace->get_size();
    do {
        // Get the link type (non-default based on random chance and threshold).
        Type link_type = get_link_type();

        // Get the poisson-distributed outgoing arity.
        size_t arity = (*_poisson_distribution)(*_random_generator);
        if (arity == 0)
            arity = 1;

        // AtomSpace will throw if the context link has bad arity
        if (link_type == CONTEXT_LINK)
            arity = 2;

        // Generate the outgoing sequence.
        HandleSeq outgoing;
        for (size_t outgoing_count = 0; outgoing_count < arity; outgoing_count++) {

            // Get a new random handle.
            Handle candidate = get_random_handle();

            // Test to see if it is new for this outgoing sequence.
            while (sequence_contains(outgoing, candidate))
                candidate = get_random_handle();
            
            // Add this candidate to our outgoing sequence.
            outgoing.push_back(candidate);
        }

        // Add the link to the atomspace.
        link = _atomspace->add_link(link_type, outgoing);

    // Until we've actually added a link.
    } while (_atomspace->get_size() == initial_atom_count);

    // Set the truth value (non-default based on random chance and threshold).
    set_truth_value(link);
}

void RandomAtomGenerator::make_random_atoms(long total_atoms,
                                            float percent_links)
{
    // Add the nodes.
    int total_nodes = total_atoms * (1.0f - percent_links);
    for (int node_count = 0; node_count < total_nodes; node_count++)
        make_random_node();

    // Remember the last UUID which is used to generate the outgoing set
    // in the links.
    _last_out_UUID = TLB::getMaxUUID();

    // Add the links until we get to to our total.
    int total_links = total_atoms - total_nodes;
    for (int link_count = 0; link_count < total_links; link_count++)
        make_random_link();

    // Remember the new last outgoing UUID.
    _last_out_UUID = TLB::getMaxUUID();
}

} // namespace opencog
