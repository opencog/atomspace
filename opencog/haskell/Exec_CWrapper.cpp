
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
                 , char** v_type
                 , double* parameters)
{
    Handle h = *handle;
    ValuePtr v = EvaluationLink::do_evaluate(atomspace, h);
    if (!v->is_type(FLOAT_VALUE))
        throw RuntimeException(TRACE_INFO,
                               "Unexpected value type: %s, FloatValue is expected",
                               nameserver().getTypeName(v->get_type()).c_str());
    return FloatValue_toRaw(FloatValueCast(v),v_type,parameters);
}


