#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/atoms/core/FunctionLink.h>

#include "BindlinkStub.h"

using namespace opencog;

Handle opencog::do_execute(AtomSpace* atomspace, Handle handle)
{
    FunctionLinkPtr flp(FunctionLinkCast(handle));
    if (flp) return HandleCast(flp->execute());
    return Handle();
}
