
#include <opencog/atomspace/AtomSpace.h>

extern "C"
{
    using namespace opencog;

    AtomSpace* AtomSpace_new();
    void AtomSpace_delete( AtomSpace* this_ptr );

    long AtomSpace_addNode( AtomSpace* this_ptr
                          , const char* type
                          , const char* name);

    long AtomSpace_addLink( AtomSpace* this_ptr
                          , const char* type
                          , const long* outgoing
                          , int size );

    long AtomSpace_getNode( AtomSpace* this_ptr
                          , const char* type
                          , const char* name
                          , int* found );

    long AtomSpace_getLink( AtomSpace* this_ptr
                          , const char* type
                          , const long* outgoing
                          , int size
                          , int* found );

    int AtomSpace_removeAtom( AtomSpace* this_ptr
                            , long handle );

    void AtomSpace_debug( AtomSpace* this_ptr );

    int AtomSpace_getTruthValue( AtomSpace* this_ptr
                               , long handle
                               , double* parameters );

    void AtomSpace_setTruthValue( AtomSpace* this_ptr
                               , long handle
                               , int type
                               , double* parameters );
}

