
#include <opencog/atomspace/AtomSpace.h>

extern "C"
{
    using namespace opencog;

    AtomSpace* AtomSpace_new();
    void AtomSpace_delete( AtomSpace* this_ptr );

    UUID AtomSpace_addNode( AtomSpace* this_ptr
                          , const char* type
                          , const char* name);

    UUID AtomSpace_addLink( AtomSpace* this_ptr
                          , const char* type
                          , const UUID* outgoing
                          , int size );

    UUID AtomSpace_getNode( AtomSpace* this_ptr
                          , const char* type
                          , const char* name
                          , int* found );

    UUID AtomSpace_getLink( AtomSpace* this_ptr
                          , const char* type
                          , const UUID* outgoing
                          , int size
                          , int* found );

    int AtomSpace_removeAtom( AtomSpace* this_ptr
                            , UUID handle );

    void AtomSpace_debug( AtomSpace* this_ptr );

    TruthValueType AtomSpace_getTruthValue( AtomSpace* this_ptr
                                          , UUID handle
                                          , double* parameters );

    void AtomSpace_setTruthValue( AtomSpace* this_ptr
                                , UUID handle
                                , TruthValueType type
                                , double* parameters );
}

