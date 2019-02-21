#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/value/Value.h>

#include "BindlinkStub.h"

using namespace opencog;

// XXX FIXME -- signature should be
// ValuePtr opencog::do_execute(AtomSpace* atomspace, Handle h)
Handle opencog::do_execute(AtomSpace* atomspace, Handle h)
{
	Instantiator inst(atomspace);
	ValuePtr pap(inst.execute(h));
	return HandleCast(pap);
}
