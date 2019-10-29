#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/value/Value.h>

#include "BindlinkStub.h"

using namespace opencog;

ValuePtr opencog::do_execute(AtomSpace* atomspace, Handle h)
{
	Instantiator inst(atomspace);
	ValuePtr pap(inst.execute(h));
	if (pap and pap->is_atom())
		return atomspace->add_atom(HandleCast(pap));
	return pap;
}
