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

// Need a lock to protect the map, since multiple threads may be trying
// to update this map.  The map contains a use-count for the number of
// threads that are currently using this atomspace as the current
// atomspace.  When the count drops to zero, the atomspace will be
// reaped if the guile GC finds that the number of SCM references
// has also dropped to zero.

std::mutex SchemeSmob::as_mtx;
std::map<AtomSpace*, int> SchemeSmob::deleteable_as;

/* ============================================================== */

std::string SchemeSmob::as_to_string(const AtomSpace *as)
{
#define BUFLEN 120
	char buff[BUFLEN];

	snprintf(buff, BUFLEN, "#<atomspace %p>", as);
	return buff;
}

/* ============================================================== */
/**
 * Create SCM object wrapping the atomspace.
 * Guile will try to memory-manage it; the deleteable_as will be
 * used to track foreign vs. internal references.
 */
SCM SchemeSmob::make_as(AtomSpace *as)
{
	SCM smob;
	SCM_NEWSMOB (smob, cog_misc_tag, as);
	SCM_SET_SMOB_FLAGS(smob, COG_AS);
	return smob;
}

/* ============================================================== */

/**
 * The only place this is called from is the guile garbage collector!
 * Its called when guile thinks it has no pointers to the given
 * atomspace; thus, we should free it. In fact, we only decrement the
 * use count for it; XXX but under what circumstance would that not be
 * zero? If its racing with another thread ?? I'm confused.
 *
 * Note that if the atomspace was given to us externally, e.g. by the
 * cogserver, then it will not even have a use-count (and thus, it's
 * never deleted here.)
 */
void SchemeSmob::release_as (AtomSpace *as)
{
	std::unique_lock<std::mutex> lck(as_mtx);
	auto has = deleteable_as.find(as);
	if (deleteable_as.end() == has) return;

	if (0 == deleteable_as[as])
	{
		AtomSpace* env = as->get_environ();
		deleteable_as.erase(has);
		lck.unlock();
		delete as;
		lck.lock();

		// (Recursively) decrement the use count on the parent.
		// We had incremented it earlier, in `ss_new_as`, when
		// creating it. Do not delete it, unless the guile gc
		// asks us to.
		while (env and deleteable_as.end() != deleteable_as.find(env))
		{
			if (0 < deleteable_as[env])
				--deleteable_as[env];
			env = env->get_environ();
		}
	}
}

/* ============================================================== */
/**
 * Create a new atom space.  The parent argument might not
 * be present -- its optional.
 */
SCM SchemeSmob::ss_new_as (SCM s)
{
	AtomSpace *parent = ss_to_atomspace(s);

	AtomSpace *as = new AtomSpace(parent);

	scm_gc_register_allocation(sizeof(*as));

	// Only the internally-created atomspaces are trackable.
	std::lock_guard<std::mutex> lck(as_mtx);
	deleteable_as[as] = 0;

	// Guile might have discarded all references to the parent; but
	// we must not delete it, as long as a child is still referencing
	// it.  The issue is that guile-gc does not know about this
	// reference, so we have to track it manually.
	while (parent and deleteable_as.end() != deleteable_as.find(parent))
	{
		deleteable_as[parent]++;
		parent = parent->get_environ();
	}

	return make_as(as);
}

/* ============================================================== */
/**
 * Return true if the scm is an atom space
 */
SCM SchemeSmob::ss_as_p (SCM s)
{
	if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, s))
	{
		scm_t_bits misctype = SCM_SMOB_FLAGS(s);
		switch (misctype)
		{
			case COG_AS:
				return SCM_BOOL_T;

			default:
				return SCM_BOOL_F;
		}
	}
	return SCM_BOOL_F;
}

/* ============================================================== */
/* Cast SCM to atomspace */

AtomSpace* SchemeSmob::ss_to_atomspace(SCM sas)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sas))
		return nullptr;

	scm_t_bits misctype = SCM_SMOB_FLAGS(sas);
	if (COG_AS != misctype)
		return nullptr;

	AtomSpace* as = (AtomSpace *) SCM_SMOB_DATA(sas);
	scm_remember_upto_here_1(sas);
	return as;
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

	AtomSpace* env = as->get_environ();
	scm_remember_upto_here_1(sas);
	return env ? make_as(env) : SCM_EOL;
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
 */
SCM SchemeSmob::ss_as(SCM satom)
{
	Handle h(scm_to_handle(satom));
	if (nullptr == h)
		scm_wrong_type_arg_msg("cog-as", 1, satom, "opencog atom");

	AtomSpace* as = h->getAtomSpace();
	if (nullptr == as) return SCM_EOL;

	return make_as(as);
}

/* ============================================================== */
/**
 * Return current atomspace for this dynamic state.
 */
SCM SchemeSmob::atomspace_fluid;

SCM SchemeSmob::ss_get_as (void)
{
	return scm_fluid_ref(atomspace_fluid);
}

/// The current atomspace for the current thread must not be deleted
/// under any circumstances (even if guile thinks that there are no
/// references to it). Thus, each thread increments a use-count on the
/// atomspace (the use-count is stored in deleteable_as).  When the
/// guile-gc finds atomspaces that have no SCM smobs pointing at them,
/// it will call `release_as()` to delete them. If those atomspaces
/// also have a zero use-count (because no threads are using them),
/// then we can safely delete them (and the atoms they contain).
///
/// Only atomspaces that were internally created (i.e. created by a
/// scheme call) are eligible for deletion.  We may also be given
/// atomspaces that magically appeared from the outside world --- we
/// do NOT track those for deletion (nor do we delete them).
///
/// ... However, a common scenario seems to be that the new atomspace
/// is a temp atomspace, created in the pattern matcher, the pattern
/// minor or somewhere else, and it's parent is the old atomspace
/// (which we are tracking). That means that the old atomspace is still
/// in use: its the parent of an untracked child. So we need to
/// increment in this case as well, lest the use-count drop to zero.
///
void SchemeSmob::as_ref_count(SCM old_as, AtomSpace *nas)
{
	AtomSpace* oas = ss_to_atomspace(old_as);
	if (oas == nas) return;

	std::lock_guard<std::mutex> lck(as_mtx);
	if (deleteable_as.end() != deleteable_as.find(nas))
	{
		++deleteable_as[nas];
	}
	else
	{
		AtomSpace* env = nas->get_environ();
		if (env and deleteable_as.end() != deleteable_as.find(env))
			++deleteable_as[env];
	}

	if (oas)
	{
		if (deleteable_as.end() != deleteable_as.find(oas))
		{
			if (0 < deleteable_as[oas])
				--deleteable_as[oas];
		}
		else
		{
			AtomSpace* env = oas->get_environ();
			if (env and deleteable_as.end() != deleteable_as.find(env))
			{
				if (0 < deleteable_as[env])
					--deleteable_as[env];
			}
		}
	}
}

/**
 * Set the current atomspace for this dynamic state.
 * Return the previous atomspace.
 */
SCM SchemeSmob::ss_set_as (SCM new_as)
{
	AtomSpace* nas = ss_to_atomspace(new_as);
	if (!nas)
		return SCM_BOOL_F;

	SCM old_as = ss_get_as();
	as_ref_count(old_as, nas);

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
	// Do NOT do the following: it is tempting, but wrong.
	// as_ref_count(ss_get_as(), nas);
	// Why? Because this function is called from the evaluator, only,
	// and it's likely that the use-count on "saved_as" will drop to
	// zero, which would be undesirable. At any rate, the calls to
	// this function always come in matched pairs, so its pointless
	// to do more than the minimum amount of work.

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
 * Throw errors if the list is not stictly just key-value pairs
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
