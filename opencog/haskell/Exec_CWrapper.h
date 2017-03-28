
#include <opencog/atomspace/AtomSpace.h>

extern "C"
{
    using namespace opencog;

    int Exec_execute(AtomSpace* atomspace, Handle* handle,Handle* out);

    int Exec_evaluate(AtomSpace* atomspace
                     , Handle* handle
                     , char** tv_type
                     , double* parameters);
}
