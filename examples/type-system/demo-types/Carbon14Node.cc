/*
 * Carbon14Node.cc
 *
 * Example of an an "active" Atom that can "do something" when
 * it is executed.
 */

#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include "Carbon14Node.h"

using namespace opencog;

ValuePtr Carbon14Node::execute(AtomSpace* as, bool silent)
{
	printf("hello world!\n");

	ValueSeq decay_products;
	ValuePtr vp = createLinkValue(decay_products);
	return vp;
}

// Boilerplate.  Leave this alone.
DEFINE_NODE_FACTORY(Carbon14Node, CARBON14_NODE)
