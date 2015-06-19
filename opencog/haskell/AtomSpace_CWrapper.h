
#include <opencog/atomspace/AtomSpace.h>

/**
 * C wrapper of the AtomSpace:
 * It was developed as an interface necessary for haskell bindings.
 * (ghc supports FFI for c libraries)
 * Now, it doesn't have specific code related to Haskell, so this library
 * could be used by another application with same requirements.
 */

extern "C"
{
    using namespace opencog;

    /**
     * AtomSpace_new Creates a new instance of the AtomSpace class.
     *
     * @return  Pointer to the AtomSpace instance created.
     */
    AtomSpace* AtomSpace_new();

    /**
     * AtomSpace_delete Deletes an AtomSpace object.
     *
     * @param this_ptr  Pointer to object.
     */
    void AtomSpace_delete( AtomSpace* this_ptr );

    /**
     * AtomSpace_addNode Inserts a new node to the atomspace, or
     *                   updates it if exists.
     *
     * @param  this_ptr  Pointer to AtomSpace instance.
     * @param  type      String representation of a node type.
     * @param  name      Node name.
     *
     * @return Handle id of the node inserted.
     */
    UUID AtomSpace_addNode( AtomSpace* this_ptr
                          , const char* type
                          , const char* name);

    /**
     * AtomSpace_addLink Inserts a new link to the atomspace, or
     *                   updates it if exists.
     *
     * @param  this_ptr  Pointer to AtomSpace instance.
     * @param  type      String representation of a link type.
     * @param  outgoing  List of UUID of the outgoing set.
     * @param  size      Size of the outgoing list.
     *
     * @return Handle id of the link inserted.
     */
    UUID AtomSpace_addLink( AtomSpace* this_ptr
                          , const char* type
                          , const UUID* outgoing
                          , int size );

    /**
     * AtomSpace_getNode Gets a node back from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a node type.
     * @param      name      Node name.
     * @param[out] found     Flag to know if the node was found.
     *
     * @return     Handle id of the node.
     */
    UUID AtomSpace_getNode( AtomSpace* this_ptr
                          , const char* type
                          , const char* name
                          , int* found );

    /**
     * AtomSpace_getLink     Gets a link back from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a link type.
     * @param      outgoing  List of UUID of the outgoing set.
     * @param      size      Size of the outgoing list.
     * @param[out] found     Flag to know if the link was found.
     *
     * @return     Handle id of the link.
     */
    UUID AtomSpace_getLink( AtomSpace* this_ptr
                          , const char* type
                          , const UUID* outgoing
                          , int size
                          , int* found );

    /**
     * AtomSpace_removeAtom  Removes an atom from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      handle    Handle id of the atom to be removed.
     *
     * @return     Flag to know if the atom has been removed.
     */
    int AtomSpace_removeAtom( AtomSpace* this_ptr
                            , UUID handle );

    /**
     * AtomSpace_debug  Debug function to print the state
     *                  of the atomspace on stderr.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     */
    void AtomSpace_debug( AtomSpace* this_ptr );

    /**
     * AtomSpace_getTruthValue  Gets the truthvalue of
     *                          an atom on the atomspace.
     *
     * @param      this_ptr    Pointer to AtomSpace instance.
     * @param      handle      Handle id of target atom.
     * @param[out] parameters  List of parameters of TruthValue result instance.
     *
     * @return     TruthValue type.
     */
    TruthValueType AtomSpace_getTruthValue( AtomSpace* this_ptr
                                          , UUID handle
                                          , double* parameters );

    /**
     * AtomSpace_setTruthValue  Sets the truthvalue of
     *                          an atom on the atomspace.
     *
     * @param      this_ptr    Pointer to AtomSpace instance.
     * @param      handle      Handle id of target atom.
     * @param      type        TruthValue type to be set.
     * @param      parameters  List of parameters of TruthValue to be set.
     */
    void AtomSpace_setTruthValue( AtomSpace* this_ptr
                                , UUID handle
                                , TruthValueType type
                                , double* parameters );
}

