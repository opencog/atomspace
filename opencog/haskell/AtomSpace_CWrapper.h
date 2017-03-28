
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
                         , Handle* uuid_out );

    /**
     * AtomSpace_addLink Inserts a new link to the atomspace, or
     *                   updates it if exists.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a link type.
     * @param      outgoing  List of Handle of the outgoing set.
     * @param      size      Size of the outgoing list.
     * @param[out] uuid_out  Uuid of the link inserted.
     *
     * @return  0 (success) if the link was inserted.
     */
    int AtomSpace_addLink( AtomSpace* this_ptr
                         , const char* type
                         , const Handle** outgoing
                         , int size
                         , Handle* uuid_out );

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
                         , Handle* uuid_out );

    /**
     * AtomSpace_getLink     Gets a link back from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      type      String representation of a link type.
     * @param      outgoing  List of Handle of the outgoing set.
     * @param      size      Size of the outgoing list.
     * @param[out] uuid_out  Uuid of the link (when found).
     *
     * @return  0 (success) if the link was found.
     */
    int AtomSpace_getLink( AtomSpace* this_ptr
                         , const char* type
                         , const Handle** outgoing
                         , int size
                         , Handle* uuid_out );

    /**
     * AtomSpace_removeAtom  Removes an atom from the atomspace.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     * @param      uuid      Uuid of the atom to be removed.
     *
     * @return  0 (success) if the link was removed.
     */
    int AtomSpace_removeAtom( AtomSpace* this_ptr
                            , Handle* uuid );

    /**
     * AtomSpace_getAtomByHandle Gets an atom back from the atomspace.
     * XXX FIXME no one should be using Handle's to work with atoms,
     * except for the database and communications back-ends.  The
     * Handle's were never intended as a user interface to atoms, and,
     * in particular, language bindings should not be using them.
     *
     * @param      this_ptr      Pointer to AtomSpace instance.
     * @param      uuid          Handle of the atom.
     * @param      node_or_link  =1 if node, =0 if link.
     * @param[out] type          String representation of atom type.
     * @param[out] name          String representation of atom name (if node).
     * @param[out] out           List of Handle of the outgoing set (if link).
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
    int AtomSpace_getAtomByHandle( AtomSpace* this_ptr
                                 , Handle* handle
                                 , int* node_or_link
                                 , char** type
                                 , char** name
                                 , Handle** out
                                 , int* out_len);

    int AtomSpace_getAtom( AtomSpace * this_ptr
                         , Handle* id
                         , const char * name
                         , const char * type
                         , size_t * size
                         , Handle* outsetp);

    /**
     * AtomSpace_debug  Debug function to print the state
     *                  of the atomspace on stderr.
     *
     * @param      this_ptr  Pointer to AtomSpace instance.
     */
    void AtomSpace_debug( AtomSpace* this_ptr );

}

