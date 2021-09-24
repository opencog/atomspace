#ifndef _OPENCOG_RANDOM_ATOM_GENERATOR_H
#define _OPENCOG_RANDOM_ATOM_GENERATOR_H

#include <random>
#include <boost/tuple/tuple.hpp>

#include <opencog/util/mt19937ar.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/types.h>


namespace opencog
{

#define USE_TIME_RANDOM_SEED 0

class RandomAtomGenerator
{
    AtomSpace* _atomspace;

    Type _default_node_type;
    float _chance_of_non_default_node;
    Type _default_link_type;
    float _chance_of_non_default_link;
    float _link_size_mean;

    int _total_types;

    int _counter;
    float _chance_of_default_tv;

    unsigned long _random_seed;
    MT19937RandGen* _random_generator;
    std::poisson_distribution<unsigned>* _poisson_distribution;

    // Generate a random type that is a subclass of the supplied parent type.
    Type random_type(Type parent_type);

    // Set the truth value for the atom using chance defaults and threshold.
    void set_truth_value(Handle& atom);

    // Does the sequence contain the handle?
    bool sequence_contains(HandleSeq& sequence, Handle& target);

    // Node functions

    // Make a random node of name "node <counter>" where counter
    // is incremented for each node.
    void make_random_node();

    // Get a node type using chance defaults and threshold.
    Type get_node_type();


    // Link functions

	// Make a random link with random arity and poisson distributed outgoings.
	void make_random_link();

    // Get a link type using chance defaults and threshold.
    Type get_link_type();

    // Get a random handle
    Handle get_random_handle();

public:
    RandomAtomGenerator(AtomSpace* atomspace,
                        unsigned long random_seed = USE_TIME_RANDOM_SEED,
                        float link_size_mean = 2.0f,
                        Type default_node_type = CONCEPT_NODE,
                        float chance_of_non_default_node = 0.4f,
                        Type default_link_type = ORDERED_LINK,
                        float chance_of_non_default_link = 0.4f,
                        float chance_of_default_tv = 0.8f);
    ~RandomAtomGenerator();

    void make_random_atoms(long total_atoms,
                           float percent_links);

};

} // namespace opencog

#endif //_OPENCOG_RANDOM_ATOM_GENERATOR_H
