#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/Value.h>

#include "BindlinkStub.h"

using namespace opencog;

Handle opencog::do_execute(AtomSpace* atomspace, Handle handle)
{
    return HandleCast(handle->execute(atomspace));
}
