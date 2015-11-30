
#include "Utils_CWrapper.h"
#include "Exec_CWrapper.h"
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>

/**
 * See ExecSCM
 */
UUID Exec_execute(AtomSpace* atomspace, UUID handle)
{
    Handle h(handle);
	Instantiator inst(atomspace);
	Handle rh(inst.execute(h));
	if (NULL != rh)
		rh = atomspace->add_atom(rh);
	return rh.value();
}

/**
 * See ExecSCM
 */
int Exec_evaluate(AtomSpace* atomspace
                 , UUID handle
                 , TruthValueType* tv_type
                 , double* parameters)
{
    Handle h(handle);
	TruthValuePtr tv = EvaluationLink::do_evaluate(atomspace, h);
    return Utils_toRawType(tv,tv_type,parameters);
}


