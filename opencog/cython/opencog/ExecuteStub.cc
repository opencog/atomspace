#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/Value.h>

#include "ExecuteStub.h"

using namespace opencog;

ValuePtr opencog::do_execute(AtomSpace* atomspace, Handle h)
{
	if (not h->is_executable())
		return h;

	ValuePtr pap(h->execute(atomspace));
	if (pap and pap->is_atom())
		return atomspace->add_atom(HandleCast(pap));
	return pap;
}
