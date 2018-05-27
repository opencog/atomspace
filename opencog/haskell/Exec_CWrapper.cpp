
#include "Exec_CWrapper.h"
#include "Value_CWrapper.h"
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>

/**
 * See ExecSCM
 */
int Exec_execute(AtomSpace* atomspace, Handle* handle,Handle* out)
{
    Handle h = *handle;
    Instantiator inst(atomspace);
    Handle rh(HandleCast(inst.execute(h)));
    if (nullptr != rh) {
		rh = atomspace->add_atom(rh);
        *out = rh;
        return 0;
    } else {
        return 1;
    }
}

/**
 * See ExecSCM
 */
int Exec_evaluate(AtomSpace* atomspace
                 , Handle* handle
                 , char** tv_type
                 , double* parameters)
{
    Handle h = *handle;
	TruthValuePtr tv = EvaluationLink::do_evaluate(atomspace, h);
    return FloatValue_toRaw(tv,tv_type,parameters);
}


