/*
 * SchemeSmobAS.c
 *
 * Scheme small objects (SMOBS) for atom spaces.
 *
 * Copyright (c) 2008,2009,2014 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/util/oc_assert.h>

using namespace opencog;

/* ============================================================== */
/**
 * Create SCM object wrapping the atomspace.
 */
SCM SchemeSmob::make_as(const AtomSpacePtr& asp)
{
	return protom_to_scm(asp);
}

/* ============================================================== */
/**
 * Create a new atom space.  The parent argument might not
 * be present -- its optional.
 * If it is present, it might be a list of one or more atomspaces.
 * If the first in the list is a string, then it is the string name
 * of the new AtomSpace.
 */
SCM SchemeSmob::ss_new_as (SCM space_list)
{
	// The first item in the list might be the name of the AtomSpace.
	std::string name;
	if (not scm_is_null(space_list) and scm_is_pair(space_list))
	{
		SCM sname = SCM_CAR(space_list);
		if (scm_is_string(sname))
		{
			char * cname = scm_to_utf8_string(sname);
			name = cname;
			free(cname);

			space_list = SCM_CDR(space_list);
		}
	}

	HandleSeq spaces;
	spaces = verify_handle_list_msg(space_list, "cog-new-atomspace", 1,
		"a list of AtomSpaces", "an AtomSpace");

	AtomSpacePtr as = createAtomSpace(spaces);

	if (0 < name.size())
		as->set_name(name);

	return protom_to_scm(as);
}

/* ============================================================== */
/**
 * Add an AtomSpace to the current AtomSpace. Unlike ss_new_node,
 * the ss_new_as() call above does NOT add the new AtomSpace to any
 * existing AtomSpace. It just ... hangs there in the void. This is
 * usually just fine, and is the historic behavior. But in order to
 * get naming uniqueness guarantees, it has to be actually inserted
 * somewhere, where naming uniqueness is provided. So that's what this
 * method does.
 *
 * Note that this might be crazy-making for a naive user: if two
 * atomspaces are created with exactly the same name, and both are
 * inserted, the second insert will return the first atomspace.
 * Which is exactly the whole point of this function; but it also
 * means that the second atomspace might be garbage-collected, which
 * might come as a surprise to the user, esp. if it isn't empty.
 */
SCM SchemeSmob::ss_add_as (SCM sas)
{
	Handle hasp(verify_handle(sas, "cog-add-atomspace"));

	if (hasp->get_type() != ATOM_SPACE)
		scm_wrong_type_arg_msg("cog-add-atomspace", 1, sas, "opencog atomspace");

	const AtomSpacePtr& asp = ss_get_env_as("cog-add-atomspace");
	Handle h(asp->add_atom(hasp));
	return handle_to_scm(h);
}

/* ============================================================== */
/**
 * Return true if the scm is an AtomSpace
 */
SCM SchemeSmob::ss_as_p (SCM s)
{
	ValuePtr vp(scm_to_protom(s));
	if (nullptr == vp)
		return SCM_BOOL_F;

	if (ATOM_SPACE == vp->get_type())
		return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/* ============================================================== */
/* Cast SCM to atomspace */

const AtomSpacePtr& SchemeSmob::ss_to_atomspace(SCM sas)
{
	static AtomSpacePtr nullasp;
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sas))
		return nullasp;

	scm_t_bits misctype = SCM_SMOB_FLAGS(sas);
	if (COG_PROTOM != misctype) // Should this be a wrong-type-arg?
		return nullasp;

	const AtomSpacePtr& asp = *(SCM_SMOB_AS_PTR_LOC(sas));
	if (ATOM_SPACE != asp->get_type())
		return nullasp;

	return asp;
}

/* ============================================================== */

AtomSpace* SchemeSmob::verify_atomspace(SCM sas, const char * subrname, int pos)
{
	const AtomSpacePtr& asp = ss_to_atomspace(sas);
	if (nullptr == asp)
		scm_wrong_type_arg_msg(subrname, pos, sas, "opencog atomspace");

	return asp.get();
}

/* ============================================================== */
/**
 * Return UUID of the atomspace
 */
SCM SchemeSmob::ss_as_uuid(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-uuid");

	UUID uuid = asp->get_uuid();
	scm_remember_upto_here_1(sas);

	return scm_from_ulong(uuid);
}

/* ============================================================== */
/**
 * Return parent of the atomspace, or an empty list if there is no parent
 */
SCM SchemeSmob::ss_as_env(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-env");

	const HandleSeq& oset = asp->getOutgoingSet();
	SCM list = SCM_EOL;
	for (size_t i = oset.size(); i > 0; i--)
	{
		SCM smob = handle_to_scm(oset[i-1]);
		list = scm_cons (smob, list);
	}

	return list;
}

/* ============================================================== */
/**
 * Return readonly flag of the atomspace.  If no atomspace specified,
 * then get the current atomspace.
 */
SCM SchemeSmob::ss_as_readonly_p(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-readonly?");
	scm_remember_upto_here_1(sas);

	if (asp->get_read_only()) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/**
 * Return COW flag of the atomspace.  If no atomspace specified,
 * then get the current atomspace.
 */
SCM SchemeSmob::ss_as_cow_p(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-cow?");

	if (asp->get_copy_on_write()) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/* ============================================================== */
/**
 * Set the readonly flag of the atomspace.  If no atomspace specified,
 * then set it on the current atomspace.  XXX This is a temporary hack,
 * until a better permission system is invented. XXX FIXME.
 */
SCM SchemeSmob::ss_as_mark_readonly(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-ro!");

	asp->set_read_only();
	return SCM_BOOL_T;
}

SCM SchemeSmob::ss_as_mark_readwrite(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-rw!");

	asp->set_read_write();
	return SCM_BOOL_T;
}

SCM SchemeSmob::ss_as_mark_cow(SCM scow, SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-cow!");

	bool cow = scm_to_bool(scow);
	if (cow) asp->set_copy_on_write();
	else asp->clear_copy_on_write();
	return SCM_BOOL_T;
}

/* ============================================================== */
/**
 * Clear the atomspace
 */
SCM SchemeSmob::ss_as_clear(SCM sas)
{
	const AtomSpacePtr& asg = ss_to_atomspace(sas);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-atomspace-clear");

	asp->clear();

	scm_remember_upto_here_1(sas);
	return SCM_BOOL_T;
}

/* ============================================================== */
/**
 * Return the atomspace of an atom.
 * If no atom, return the current atomspace.
 */
SCM SchemeSmob::ss_as(SCM satom)
{
	// If no argument, then return the current AtomSpace.
	Handle h(scm_to_handle(satom));
	if (nullptr == h)
	{
		const AtomSpacePtr& asp = ss_get_env_as("cog-atomspace");
		return asp ? make_as(asp) : SCM_EOL;
	}

	AtomSpace* as = h->getAtomSpace();
	return as ? make_as(AtomSpaceCast(as->shared_from_this())) : SCM_EOL;
}

/* ============================================================== */
/**
 * Return current atomspace for this dynamic state.
 */
SCM SchemeSmob::atomspace_fluid;

/**
 * Set the current atomspace for this dynamic state.
 * Return the previous atomspace.
 */
SCM SchemeSmob::ss_set_as (SCM new_as)
{
	const AtomSpacePtr& nas = ss_to_atomspace(new_as);
	if (!nas)
		return SCM_BOOL_F;

	SCM old_as = scm_fluid_ref(atomspace_fluid);

	scm_fluid_set_x(atomspace_fluid, new_as);

	return old_as;
}

/* ============================================================== */
/**
 * Set the atomspace into the top-level interaction environment
 * Since its held in a fluid, it can have a different value in each
 * thread, so that different threads can use different atomspaces,
 * all at the same time.
 */
void SchemeSmob::ss_set_env_as(const AtomSpacePtr& nas)
{
	scm_fluid_set_x(atomspace_fluid, make_as(nas));
}

const AtomSpacePtr& SchemeSmob::ss_get_env_as(const char* subr)
{
	SCM ref = scm_fluid_ref(atomspace_fluid);
	const AtomSpacePtr& asp = ss_to_atomspace(ref);
	return asp;
}

/* ============================================================== */

/**
 * Search for an atomspace in a list of values.
 * Return the atomspace if found, else return null.
 * Throw errors if the list is not stictly just key-value pairs.
 */
const AtomSpacePtr& SchemeSmob::get_as_from_list(SCM slist)
{
	while (scm_is_pair(slist))
	{
		SCM sval = SCM_CAR(slist);
		const AtomSpacePtr& asp = ss_to_atomspace(sval);
		if (asp) return asp;
		slist = SCM_CDR(slist);
	}

	static AtomSpacePtr nullasp;
	return nullasp;
}

/* ===================== END OF FILE ============================ */
