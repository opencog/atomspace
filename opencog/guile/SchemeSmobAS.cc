/*
 * SchemeSmobAS.c
 *
 * Scheme small objects (SMOBS) for atom spaces.
 *
 * Copyright (c) 2008,2009,2014 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_GUILE

#include <cstddef>
#include <libguile.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

// Need a lock to protect the map, since multiple threads may be trying
// to update this map.  The map contains a use-count for the number of
// threads that are currently using this atomspace as the current
// attomspace.  When the count drops to zero, the atomspace will be
// reaped if the number of SCM references also drops to zero (and the
// GC runs).
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
 * Do NOT take over memory management of it!
 */
SCM SchemeSmob::make_as (AtomSpace *as)
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
 * Note that if the atomspace was given to use externally, e.g. by the
 * cogserver, then it will not even have a use-count (and thus, it's
 * never deleted here.)
 */
void SchemeSmob::release_as (AtomSpace *as)
{
	std::lock_guard<std::mutex> lck(as_mtx);
	auto has = deleteable_as.find(as);
	if (deleteable_as.end() == has) return;
	deleteable_as[as] --;

	if (0 == deleteable_as[as])
	{
		AtomSpace* env = as->get_environ();
		deleteable_as.erase(has);
		scm_gc_unregister_collectable_memory (as,
	                  sizeof(*as), "opencog atomspace");
		delete as;

		// (Recursively) decrement the use count on the parent.
		// XXX This is confusing, I'm not sure its right.
		if (env) release_as(env);
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

	scm_gc_register_collectable_memory (as,
	                 sizeof(*as), "opencog atomspace");

	// Only the internally-created atomspaces are trackable.
	std::lock_guard<std::mutex> lck(as_mtx);
	deleteable_as[as] = 1;

	// Don't delete the parent, as long as its in use by a child
	// (that guile still has a reference to).
	while (parent and deleteable_as.end() != deleteable_as.find(parent))
	{
		deleteable_as[parent]++;
		parent = parent->get_environ();
	}

#define WORK_AROUND_GUILE_20_GC_BUG
#ifdef WORK_AROUND_GUILE_20_GC_BUG
	// Below is a wrk-around to a bug.  You can trigger this bug
	// with the code below;  if will crash, because the initial
	// AS gets erroneously garbage-collected.  Guile is trying
	// to release the main AS every time through the loop.  I can't
	// tell why.
/******
(use-modules (opencog))
(use-modules (opencog exec))

(define  n 0)
(define (prt)
   (set! n (+ n 1))
   (format #t "yaaaa ~a ~a\n" n cog-initial-as) (usleep 200000)
   (gc)
   (format #t "post-gc ~a\n" cog-initial-as)
   (if (< n 200) (stv 1 1) (stv 0 1)))

(DefineLink
   (DefinedPredicateNode "loopy")
   (SatisfactionLink
      (SequentialAndLink
         (EvaluationLink
            (GroundedPredicateNode "scm:prt") (ListLink))
         (DefinedPredicateNode "loopy")
      )))

(cog-evaluate! (DefinedPredicateNode "loopy"))
*******/
	static bool first = true;
	if (first)
	{
		first = false;
		deleteable_as[as] = 2002001000;
	}
#endif
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
		return NULL;

	scm_t_bits misctype = SCM_SMOB_FLAGS(sas);
	if (COG_AS != misctype)
		return NULL;

	AtomSpace* as = (AtomSpace *) SCM_SMOB_DATA(sas);
	scm_remember_upto_here_1(sas);
	return as;
}

/* ============================================================== */
/**
 * Return UUID of the atomspace
 */
SCM SchemeSmob::ss_as_uuid(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as)
	{
		// Special care for atom whose atomspace was null
		if (scm_is_null(sas))
			return scm_from_ulong(Handle::INVALID_UUID);
		scm_wrong_type_arg_msg("cog-atomspace-uuid", 1, sas, "atomspace");
	}

	UUID uuid = as->get_uuid();
	scm_remember_upto_here_1(sas);

	return scm_from_ulong(uuid);
}

/* ============================================================== */
/**
 * Return parent of the atomspace
 */
SCM SchemeSmob::ss_as_env(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as)
	{
		// Special care for null atomspace
		if (scm_is_null(sas))
			return SCM_EOL;
		scm_wrong_type_arg_msg("cog-atomspace-env", 1, sas, "atomspace");
	}

	AtomSpace* env = as->get_environ();
	scm_remember_upto_here_1(sas);
	return make_as(env);
}

/* ============================================================== */
/**
 * Clear the atomspace
 */
SCM SchemeSmob::ss_as_clear(SCM sas)
{
	AtomSpace* as = ss_to_atomspace(sas);
	if (nullptr == as)
		scm_wrong_type_arg_msg("cog-atomspace-clear", 1, sas, "atomspace");

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
/// under any circumstances. Thus each thread increments a use-count
/// on the atomspace (stored in deleteable_as).  Atomspaces that are
/// not current in any thread, and also have no SCM smobs pointing
/// at them may be deleted by the gc.  This will cause atoms to
/// disappear, if the user is not careful ...
///
/// Oh, wait: only atomspaces that were internally created (i.e.
/// created by a scheme call) are eligible for deletion.  We may
/// also be given atomspaces that magically appeared from the outside
/// world --- we do NOT track those for deletion.
///
void SchemeSmob::as_ref_count(SCM old_as, AtomSpace *nas)
{
	AtomSpace* oas = ss_to_atomspace(old_as);
	if (oas != nas)
	{
		std::lock_guard<std::mutex> lck(as_mtx);
		if (deleteable_as.end() != deleteable_as.find(nas))
			deleteable_as[nas]++;
		if (oas and deleteable_as.end() != deleteable_as.find(oas))
			deleteable_as[oas] --;
	}
}

/**
 * Set the current atomspace for this dynamic state.
 * Return the previous atomspace.
 */
SCM SchemeSmob::ss_set_as (SCM new_as)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, new_as))
		return SCM_BOOL_F;

	if (COG_AS != SCM_SMOB_FLAGS(new_as))
		return SCM_BOOL_F;

	SCM old_as = ss_get_as();
	as_ref_count(old_as, ss_to_atomspace(new_as));

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
	as_ref_count(ss_get_as(), nas);

	scm_fluid_set_x(atomspace_fluid, make_as(nas));
}

AtomSpace* SchemeSmob::ss_get_env_as(const char* subr)
{
	SCM ref = scm_fluid_ref(atomspace_fluid);
	AtomSpace* as = ss_to_atomspace(ref);
	// if (NULL == as)
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


#endif /* HAVE_GUILE */
/* ===================== END OF FILE ============================ */
