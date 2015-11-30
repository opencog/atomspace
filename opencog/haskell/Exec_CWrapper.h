
#include <opencog/atomspace/AtomSpace.h>

extern "C"
{
    using namespace opencog;

    UUID Exec_execute(AtomSpace* atomspace, UUID handle);

    int Exec_evaluate(AtomSpace* atomspace
                     , UUID handle
                     , TruthValueType* tv_type
                     , double* parameters);
}
