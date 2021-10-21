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

std::string SchemeSmob::as_to_string(const AtomSpace *as)
{
#define BUFLEN 120
	char buff[BUFLEN];

	snprintf(buff, BUFLEN, "#<atomspace %lu>",
		(long unsigned int) as->get_uuid());
	return buff;
}

/* ============================================================== */
/**
 * Create SCM object wrapping the atomspace.
 */
SCM SchemeSmob::make_as(AtomSpace *as)
{
	return protom_to_scm(as->shared_from_this());
}

/* ============================================================== */
/**
 * Create a new atom space.  The parent argument might not
 * be present -- its optional.
 */
SCM SchemeSmob::ss_new_as (SCM s)
{
	AtomSpace *parent = ss_to_atomspace(s);
	AtomSpacePtr as = createAtomSpace(parent);
	return protom_to_scm(as);
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

	if (ATOMSPACE == vp->get_type())
		return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/* ============================================================== */
/* Cast SCM to atomspace */

AtomSpace* SchemeSmob::ss_to_atomspace(SCM sas)
{
	ValuePtr vp(scm_to_protom(sas));
	if (ATOMSPACE != vp->get_type())
		return nullptr;

	return (AtomSpace*) vp.get();
}

/* ============================================================== */

AtomSpace* SchemeSmob::verify_atomspace(SCM sas, const char * subrname, int pos)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as)
		scm_wrong_type_arg_msg(subrname, pos, sas, "opencog atomspace");

	return as;
}

/* ============================================================== */
/**
 * Return UUID of the atomspace
 */
SCM SchemeSmob::ss_as_uuid(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-uuid");

	UUID uuid = as->get_uuid();
	scm_remember_upto_here_1(sas);

	return scm_from_ulong(uuid);
}

/* ============================================================== */
/**
 * Return parent of the atomspace, or an empty list if there is no parent
 */
SCM SchemeSmob::ss_as_env(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-env");

	const HandleSeq& oset = as->getOutgoingSet();
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
	AtomSpace* as = ss_to_atomspace(sas);
	scm_remember_upto_here_1(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-readonly?");

	if (as->get_read_only()) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/**
 * Return COW flag of the atomspace.  If no atomspace specified,
 * then get the current atomspace.
 */
SCM SchemeSmob::ss_as_cow_p(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	scm_remember_upto_here_1(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-cow?");

	if (as->get_copy_on_write()) return SCM_BOOL_T;
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
	AtomSpace* as = ss_to_atomspace(sas);
	scm_remember_upto_here_1(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-ro!");

	as->set_read_only();
	return SCM_BOOL_T;
}

SCM SchemeSmob::ss_as_mark_readwrite(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	scm_remember_upto_here_1(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-rw!");

	as->set_read_write();
	return SCM_BOOL_T;
}

SCM SchemeSmob::ss_as_mark_cow(SCM scow, SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	scm_remember_upto_here_1(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-cow!");

	bool cow = scm_to_bool(scow);
	if (cow) as->set_copy_on_write();
	else as->clear_copy_on_write();
	return SCM_BOOL_T;
}

/* ============================================================== */
/**
 * Clear the atomspace
 */
SCM SchemeSmob::ss_as_clear(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as) as = ss_get_env_as("cog-atomspace-clear");

	as->clear();

	scm_remember_upto_here_1(sas);
	return SCM_BOOL_T;
}

/* ============================================================== */
/**
 * Return the atomspace of an atom.
 * If no atom, return the current atomspace.
 */
SCM SchemeSmob::ss_as(SCM slist)
{
	// If no argument, then return the current AtomSpace.
	if (scm_is_null(slist))
	{
		AtomSpace* as = ss_get_env_as("cog-atomspace");
		return as ? make_as(as) : SCM_EOL;
	}

	SCM satom = scm_car(slist);
	Handle h(verify_handle(satom, "cog-atomspace"));
	AtomSpace* as = h->getAtomSpace();
	return as ? make_as(as) : SCM_EOL;
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
	AtomSpace* nas = ss_to_atomspace(new_as);
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
void SchemeSmob::ss_set_env_as(AtomSpace *nas)
{
	scm_fluid_set_x(atomspace_fluid, make_as(nas));
}

AtomSpace* SchemeSmob::ss_get_env_as(const char* subr)
{
	// There are weird test-case scenarios where the fluid is not
	// initalized. Those will crash-n-burn without this test.
	if (0x0 == atomspace_fluid) return nullptr;

	SCM ref = scm_fluid_ref(atomspace_fluid);
	AtomSpace* as = ss_to_atomspace(ref);
	// if (nullptr == as)
	//	scm_misc_error(subr, "No atomspace was specified!", SCM_BOOL_F);
	return as;
}

/* ============================================================== */

/**
 * Search for an atomspace in a list of values.
 * Return the atomspace if found, else return null.
 * Throw errors if the list is not stictly just key-value pairs.
 */
AtomSpace* SchemeSmob::get_as_from_list(SCM slist)
{
	while (scm_is_pair(slist))
	{
		SCM sval = SCM_CAR(slist);
		AtomSpace* as = ss_to_atomspace(sval);
		if (as) return as;
		slist = SCM_CDR(slist);
	}

	return NULL;
}

/* ===================== END OF FILE ============================ */
