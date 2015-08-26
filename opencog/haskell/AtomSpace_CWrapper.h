
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
     * @param parent_ptr  Pointer to the parent atomspace
     *                    (null if we want a new independent atomspace).
     *
     * @return  Pointer to the AtomSpace instance created.
     */
    AtomSpace* AtomSpace_new( AtomSpace * parent_ptr );

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
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a node type.
     * @param      name      Node name.
     * @param[out] uuid_out  Uuid of the node inserted.
     *
     * @return  0 (success) if the node was inserted.
     */
    int AtomSpace_addNode( AtomSpace* this_ptr
                         , const char* type
                         , const char* name
                         , UUID* uuid_out );

    /**
     * AtomSpace_addLink Inserts a new link to the atomspace, or
     *                   updates it if exists.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a link type.
     * @param      outgoing  List of UUID of the outgoing set.
     * @param      size      Size of the outgoing list.
     * @param[out] uuid_out  Uuid of the link inserted.
     *
     * @return  0 (success) if the link was inserted.
     */
    int AtomSpace_addLink( AtomSpace* this_ptr
                         , const char* type
                         , const UUID* outgoing
                         , int size
                         , UUID* uuid_out );

    /**
     * AtomSpace_getNode Gets a node back from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a node type.
     * @param      name      Node name.
     * @param[out] uuid_out  Uuid of the node (when found).
     *
     * @return  0 (success) if the node was found.
     */
    int AtomSpace_getNode( AtomSpace* this_ptr
                         , const char* type
                         , const char* name
                         , UUID* uuid_out );

    /**
     * AtomSpace_getLink     Gets a link back from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a link type.
     * @param      outgoing  List of UUID of the outgoing set.
     * @param      size      Size of the outgoing list.
     * @param[out] uuid_out  Uuid of the link (when found).
     *
     * @return  0 (success) if the link was found.
     */
    int AtomSpace_getLink( AtomSpace* this_ptr
                         , const char* type
                         , const UUID* outgoing
                         , int size
                         , UUID* uuid_out );

    /**
     * AtomSpace_removeAtom  Removes an atom from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      uuid      Uuid of the atom to be removed.
     *
     * @return  0 (success) if the link was removed.
     */
    int AtomSpace_removeAtom( AtomSpace* this_ptr
                            , UUID uuid );

    /**
     * AtomSpace_getAtomByUUID Gets an atom back from the atomspace.
     *
     * @param      this_ptr      Pointer to AtomSpace instance.
     * @param      uuid          UUID of the atom.
     * @param      node_or_link  =1 if node, =0 if link.     
     * @param[out] type          String representation of atom type.
     * @param[out] name          String representation of atom name (if node).
     * @param[out] out           List of UUID of the outgoing set (if link).
     * @param[out] out_len       Size of the outgoing list (if link).
     *
     * @return  0 if success.
     *
     * NOTE: Memory for output parameter is allocated with malloc. The caller
     * should properly free memory in output parameters according to
     * the return value:
     *   If node -> free type and name fields.
     *   If link -> free type and out field.
     */
    int AtomSpace_getAtomByUUID( AtomSpace* this_ptr
                               , UUID handle
                               , int* node_or_link
                               , char** type
                               , char** name
                               , UUID** out
                               , int* out_len);

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
     * @param[out] tv_type     TruthValue type.
     * @param[out] parameters  List of parameters of TruthValue result instance.
     *
     * @return  0 if success.
     */
    int AtomSpace_getTruthValue( AtomSpace* this_ptr
                                          , UUID handle
                                          , TruthValueType* tv_type
                                          , double* parameters );

    /**
     * AtomSpace_setTruthValue  Sets the truthvalue of
     *                          an atom on the atomspace.
     *
     * @param      this_ptr    Pointer to AtomSpace instance.
     * @param      handle      Handle id of target atom.
     * @param      type        TruthValue type to be set.
     * @param      parameters  List of parameters of TruthValue to be set.
     *
     * @return  0 if success.
     */
    int AtomSpace_setTruthValue( AtomSpace* this_ptr
                               , UUID handle
                               , TruthValueType type
                               , double* parameters );
}

