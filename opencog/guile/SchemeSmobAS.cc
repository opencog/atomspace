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
#include <opencog/eval/FrameStack.h>
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

	// If a name was provided without any parent atomspaces,
	// check if an AtomSpace with that name already exists.
	if (0 < name.size() and 0 == spaces.size())
	{
		const AtomSpacePtr& env = ss_get_env_as("cog-new-atomspace");
		if (env)
		{
			if (env->get_name() == name)
				return make_as(env);

			// Find ALL frames; even those deep in a stack.
			HandleSeq existing;
			env->get_handles_by_type(existing, ATOM_SPACE, false);
			for (const Handle& h : existing)
			{
				if (h->get_name() == name)
					return handle_to_scm(h);
			}
		}
	}

	// Create new AtomSpace with the indicated parents.
	// This can throw an exception if the parents don't have
	// a valid form. Catch that exception and rethrow.
	AtomSpacePtr asp;
	try
	{
		asp = createAtomSpace(spaces);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-new-atomspace", space_list);
	}

	// If a name was provided, set that name.
	if (0 < name.size())
		asp->set_name(name);

	Handle hasp(HandleCast(asp));

	// Insert it nto the current AtomSpace
	const AtomSpacePtr& env = ss_get_env_as("cog-new-atomspace");
	if (env) hasp = env->add_atom(hasp);

	return protom_to_scm(hasp);
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
	return as ? make_as(AtomSpaceCast(as)) : SCM_EOL;
}

/* ============================================================== */
/**
 * Holds the current AtomSpace for this dynamic state. Mostly.
 * There is some non-obvious hackery going on here, due to the need
 * to satisfy multiple requirements and over-come C++ TLS weakness
 * and python integration.
 *
 * The first requirement is that new scheme threads get the same
 * AtomSpace as the one that created the new thread. Scheme fluids are
 * ideal for this: new threads get a copy of the creator's fluid state.
 *
 * The second requirement is that the AtomSpace be shared between scheme
 * and python, so that cross-language calls through GroundedPredicate and
 * GroundedSchema both use the same AtomSpace. This necessitates the use
 * of `opencog/eval/FrameStack.h` to share the AtomSpace across languages.
 * This works, and we are mostly happy. But ...
 *
 * FrameStack uses TLS, and TLS state is NOT copied when a new thread is
 * created. TLS does not behave like a fluid. This causes major issues:
 * A new scheme thread is created. We try to find the atomspace in TLS,
 * but its a null pointer! Aiee! So we fall back to getting it from the
 * fluid, and then tell the FrameStack what it is, so that python can
 * find it later.
 *
 * Otherwise, if get_frame() returns non-null, we assume that this is
 * some AtomSpace that python set up for us, and we just go ahead, and
 * use it. And that's all good. But we also better cache it in our fluid,
 * just in case some scheme code creates some threads.
 *
 * So, in a sense, the get_frame() is always the master authority for
 * what the AtomSpace is for this thread. Unless its nullptr, in which
 * case the fluid offers the ground-truth. Which we then immediately
 * hand over with set_frame() so that get_frame() can remain the master
 * authority. The scheme fluid just shadows what is in the frame, and
 * should always be in sync.
 *
 * Phew. It works, and there's lots of unit tests for this.
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

	// Get previous atomspace from the unified frame stack
	AtomSpacePtr old_asp = get_frame();
	SCM old_as = old_asp ? make_as(old_asp) : SCM_BOOL_F;

	// Set the new atomspace in the unified frame stack
	set_frame(nas);

	// Also set the fluid for thread inheritance
	scm_fluid_set_x(atomspace_fluid, new_as);

	return old_as;
}

/* ============================================================== */
/**
 * Set the atomspace into the unified frame stack.
 * Since its thread-local, it can have a different value in each
 * thread, so that different threads can use different atomspaces,
 * all at the same time.
 */
void SchemeSmob::ss_set_env_as(const AtomSpacePtr& nas)
{
	// nas will be nullptr during thread-exit and calling
	// set_frame() will cause TLS heartache and heap corruption.
	if (nullptr == nas) return;

	set_frame(nas);
	scm_fluid_set_x(atomspace_fluid, make_as(nas));
}

/**
 * Get current atomspace from frame stack, falling back to the
 * Guile fluid if the frame stack is empty. This provides thread
 * inheritance semantics - new threads inherit via the fluid.
 */
const AtomSpacePtr& SchemeSmob::get_current_as(void)
{
	const AtomSpacePtr& asp = get_frame();
	if (asp) return asp;

	// Frame stack is empty - inherit from the Guile fluid
	// (which propagates across thread creation)
	SCM ref = scm_fluid_ref(atomspace_fluid);
	const AtomSpacePtr& fluid_asp = ss_to_atomspace(ref);
	set_frame(fluid_asp);
	return get_frame();
}

const AtomSpacePtr& SchemeSmob::ss_get_env_as(const char* subr)
{
	return get_current_as();
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

/* ============================================================== */
/**
 * Push a new temporary atomspace onto the unified frame stack.
 * Creates a new atomspace with the current as parent.
 * Return the previous (base) atomspace.
 */
SCM SchemeSmob::ss_push_atomspace (void)
{
	// Get current atomspace (from frame stack or fluid)
	AtomSpacePtr base_as = get_current_as();

	// Create and push a new atomspace (child of current)
	push_frame();

	// Also set the fluid for thread inheritance
	const AtomSpacePtr& new_as = get_frame();
	scm_fluid_set_x(atomspace_fluid, make_as(new_as));

	// Return the previous (base) atomspace
	return base_as ? make_as(base_as) : SCM_EOL;
}

/* ============================================================== */
/**
 * Pop a temporary atomspace from the unified frame stack.
 * The pushed atomspace is cleared and removed from its parent.
 * Return unspecified.
 */
SCM SchemeSmob::ss_pop_atomspace (void)
{
	// Excplicitly decrement the use count on the old AtomSpacePtr
	// inside the old SMOB.
	SCM old_fluid = scm_fluid_ref(atomspace_fluid);
	SCM_SMOB_VALUE_PTR_LOC(old_fluid)->reset();
	scm_remember_upto_here_1(old_fluid);

	pop_frame();

	const AtomSpacePtr& current = get_frame();
	scm_fluid_set_x(atomspace_fluid, make_as(current));

	// Force GC to collect any remaining smobs that may hold references.
	// Without this, we get smobs pointing at zombie Atomspaces whose
	// dtors never run. The rocks ThreadCountUTest hits this hard.
	scm_gc();

	return SCM_UNSPECIFIED;
}

/* ===================== END OF FILE ============================ */
