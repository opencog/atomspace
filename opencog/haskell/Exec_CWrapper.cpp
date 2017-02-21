
#include "Utils_CWrapper.h"
#include "Exec_CWrapper.h"
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>

/**
 * See ExecSCM
 */
int Exec_execute(AtomSpace* atomspace, Handle* handle,Handle* out)
{
    Handle h = *handle;
	Instantiator inst(atomspace);
	Handle rh(inst.execute(h));
	if (NULL != rh) {
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
                 , Type* tv_type
                 , double* parameters)
{
    Handle h = *handle;
	TruthValuePtr tv = EvaluationLink::do_evaluate(atomspace, h);
    return Utils_toRawType(tv,tv_type,parameters);
}


