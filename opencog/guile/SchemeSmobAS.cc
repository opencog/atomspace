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

// Meta-theory.
// AtomSpaces are in general persistent, which means that, in general,
// they should not be deleted just because guile doesn't have a pointer
// to them. Unless they should be...
//
// There are two situations:
// 1) AtomSpaces that come from the outside world (e.g. from the
//    cogserver, or were created in python, or are temp scratch
//    AtomSpaces created by the pattern-matcher and the pattern-miner)
//    and are given to us to use. We MUST NOT delete these AtomSpaces,
//    when all guile references to them are gone.
//
// 2) AtomSpaces are created by the user calling the scheme function
//    `cog-new-atomspace`.  In this case, when all references are lost,
//    then this atomspace can be safely deleted. If the user decides
//    to hand off this atomspace to someone else (e.g. python), then
//    the user should be careful to retain a guile reference to it!
//
// There are several complications:
// A) In general, guile does NOT know that two different scheme SMOB's
//    happen to point at the same AtomSpace. Thus, there are cases where
//    guile deletes (GC's) a SMOB that is unused, but it is pointing at an
//    AtomSpace that is being used in some other SMOB. This is exhibited
//    in `MultiAtomSpaceUTest::test_as_of_atom_scm()` or e.g. by
//    saying `(load "as-of-atom.scm")` at the guile prompt. As a result,
//    we have to maintain reference counts independent of guile.
//
// B) In the top-level interaction environment, guile maintains a
//    history of return values of all functions that the user typed
//    in at the prompt. These might hold references to AtomSpaces that
//    are never GC'ed, because they are sitting in the history buffer.
//    Creating these is easy: just say `(cog-new-atomspace)` at the
//    guile prompt. At the end of this, you will not have any
//    references to the AtomSpaces; they will have been lost.
//    Well... almost lost.  One can still retrieve them with
//    `(use-modules (ice-9 history))` and then force GC with
//    `(clear-value-history!) (gc)`. Similar remarks apply to
//    `(cog-set-atomspace! new)` which returns the old AtomSpace as
//    the return value, thus putting the old AtomSpace in the history
//    buffer! Watch out!
//
// These complications are hit by `MultiAtomSpaceUTest`, by the
// `CythonGuile` unit test, and by the pattern-miner.
//
// ------------------------------------------------------------------
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

	snprintf(buff, BUFLEN, "#<atomspace %lu>",
		(long unsigned int) as->get_uuid());
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
	std::lock_guard<std::mutex> lck(as_mtx);
	if (deleteable_as.end() != deleteable_as.find(as))
		deleteable_as[as]++;
	return smob;
}

/* ============================================================== */

/**
 * The only place this is called from is the guile garbage collector!
 * Its called when guile thinks it has no pointers to the given
 * atomspace; thus, we should free it. In fact, we only decrement the
 * use count for it.  It can happen that the use-count is not zero
 * because guile doesn't realize thatthere is another SCM still pointing
 * at the same AtomSpace. Both MultiAtomSpaceUTest::test_as_of_atom_scm
 * and also the `CythonGuile` unit tests trigger this case.
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

	deleteable_as[as]--;
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

	{
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
		// block scope unlock mutex
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
/// miner or somewhere else, and it's parent is the old atomspace
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

	SCM old_as = scm_fluid_ref(atomspace_fluid);
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
