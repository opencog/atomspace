/*
 * SchemeSmobAtom.c
 *
 * Scheme small objects (SMOBS) for opencog atom properties
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linas@linas.org>
 */

#include <vector>

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/attentionbank/AttentionBank.h>

using namespace opencog;

/* ============================================================== */
/**
 * Verify that SCM arg is an actual opencog Handle; returning Handle
 *
 * This routine is meant for validating arguments passed into
 * guile-wrapped C++ code.
 *
 * This routine takes an SCM arg and a string subroutine name. It
 * verifies that the arg is actually a handle (and not, for example,
 * an int, string, etc.)  If its not a handle, it throws an SCM error,
 * using the subroutine name in the error message.  (Such an error is
 * caught by the shell, and printed as a stack trace at the shell
 * prompt).  If the arg is a handle, then the actual opencog handle
 * is returned.
 */

Handle SchemeSmob::verify_handle (SCM satom, const char * subrname, int pos)
{
	Handle h(scm_to_handle(satom));
	if (nullptr == h)
		scm_wrong_type_arg_msg(subrname, pos, satom, "opencog atom");

	// In the current C++ code, handles can also be pointers to
	// protoAtoms.  Howerver, in the guile wrapper, we expect all
	// handles to be pointers to atoms; use verify_protom() instead,
	// if you just want ProtoAtoms.
	if (not (h->isLink() or h->isNode()))
		scm_wrong_type_arg_msg(subrname, pos, satom, "opencog atom");

	return h;
}

ProtoAtomPtr SchemeSmob::verify_protom (SCM satom, const char * subrname, int pos)
{
	ProtoAtomPtr pv(scm_to_protom(satom));
	if (nullptr == pv)
		scm_wrong_type_arg_msg(subrname, pos, satom, "opencog value");

	return pv;
}

/* ============================================================== */
/**
 * Return the string name of the atom
 */
SCM SchemeSmob::ss_name (SCM satom)
{
	std::string name;
	Handle h = verify_handle(satom, "cog-name");
	if (h->isNode()) name = h->getName();
	SCM str = scm_from_utf8_string(name.c_str());
	return str;
}

SCM SchemeSmob::ss_type (SCM satom)
{
	Handle h = verify_handle(satom, "cog-type");
	Type t = h->getType();
	const std::string &tname = classserver().getTypeName(t);
	SCM str = scm_from_utf8_string(tname.c_str());
	SCM sym = scm_string_to_symbol(str);

	return sym;
}

SCM SchemeSmob::ss_arity (SCM satom)
{
	Handle h = verify_handle(satom, "cog-arity");
	Arity ari = 0;
	if (h->isLink()) ari = h->getArity();

	/* Arity is currently an unsigned short */
	SCM sari = scm_from_ushort(ari);
	return sari;
}

/* ============================================================== */
/* Truth value setters/getters */

SCM SchemeSmob::ss_tv (SCM satom)
{
	Handle h = verify_handle(satom, "cog-tv");
	return tv_to_scm(h->getTruthValue());
}

SCM SchemeSmob::ss_set_tv (SCM satom, SCM stv)
{
	Handle h = verify_handle(satom, "cog-set-tv!");
	TruthValuePtr tv = verify_tv(stv, "cog-set-tv!", 2);

	h->setTruthValue(tv);
	scm_remember_upto_here_1(stv);
	return satom;
}

SCM SchemeSmob::ss_merge_tv (SCM satom, SCM stv)
{
	Handle h = verify_handle(satom, "cog-merge-tv!");
	TruthValuePtr tv = verify_tv(stv, "cog-merge-tv!", 2);

	h->merge(tv);
	scm_remember_upto_here_1(stv);
	return satom;
}

// XXX FIXME -- this should NOT be a part of the API, it should be
// a utility function!
SCM SchemeSmob::ss_merge_hi_conf_tv (SCM satom, SCM stv)
{
	Handle h = verify_handle(satom, "cog-merge-hi-conf-tv!");
	TruthValuePtr tv = verify_tv(stv, "cog-merge-hi-conf-tv!", 2);

	h->merge(tv, MergeCtrl(MergeCtrl::TVFormula::HIGHER_CONFIDENCE));
	scm_remember_upto_here_1(stv);
	return satom;
}

// Increment the count, keeping mean and confidence as-is.
// Converts existing truth value to a CountTruthValue.
SCM SchemeSmob::ss_inc_count (SCM satom, SCM scnt)
{
	Handle h = verify_handle(satom, "cog-inc-count!");
	double cnt = verify_real(scnt, "cog-inc-count!", 2);

	TruthValuePtr tv = h->getTruthValue();
	if (COUNT_TRUTH_VALUE == tv->getType())
	{
		cnt += tv->getCount();
	}
	tv = CountTruthValue::createTV(
		tv->getMean(), tv->getConfidence(), cnt);

	h->setTruthValue(tv);
	return satom;
}

/* ============================================================== */
/* Attention-Value stuff */
// XXX FIXME all this should move to attentionbank/AttentionBankSCM.cc

SCM SchemeSmob::ss_av (SCM satom)
{
	Handle h = verify_handle(satom, "cog-av");
	AtomSpace* atomspace = ss_get_env_as("cog-av");

	AttentionValue *sav = attentionbank(atomspace).get_av(h)->rawclone();
	return take_av(sav);
}

SCM SchemeSmob::ss_set_av (SCM satom, SCM sav)
{
	Handle h = verify_handle(satom, "cog-set-av!");
	AttentionValue *av = verify_av(sav, "cog-set-av!", 2);
	AtomSpace* atomspace = ss_get_env_as("cog-set-av!");

	attentionbank(atomspace).change_av(h, av->clone());
	return satom;
}

SCM SchemeSmob::ss_inc_vlti (SCM satom)
{
	Handle h = verify_handle(satom, "cog-inc-vlti!");
	AtomSpace* atomspace = ss_get_env_as("cog-inc-vlti!");

	attentionbank(atomspace).inc_vlti(h);
	return satom;
}

SCM SchemeSmob::ss_dec_vlti (SCM satom)
{
	Handle h = verify_handle(satom, "cog-dec-vlti!");
	AtomSpace* atomspace = ss_get_env_as("cog-dec-vlti!");

	attentionbank(atomspace).dec_vlti(h);
	return satom;
}

/* ============================================================== */
/**
 * Convert the outgoing set of an atom into a list; return the list.
 */
SCM SchemeSmob::ss_outgoing_set (SCM satom)
{
	Handle h = verify_handle(satom, "cog-outgoing-set");

	if (not h->isLink()) return SCM_EOL;

	const HandleSeq& oset = h->getOutgoingSet();

	SCM list = SCM_EOL;
	for (int i = oset.size()-1; i >= 0; i--)
	{
		SCM smob = handle_to_scm(oset[i]);
		list = scm_cons (smob, list);
	}

	return list;
}

/* ============================================================== */
/**
 * Convert the outgoing set of an atom into a list;
 * filter-accept only type, and return the list.
 */
SCM SchemeSmob::ss_outgoing_by_type (SCM satom, SCM stype)
{
	Handle h = verify_handle(satom, "cog-outgoing-by-type");
	Type t = verify_atom_type(stype, "cog-outgoing-by-type", 2);

	if (not h->isLink()) return SCM_EOL;

	const HandleSeq& oset = h->getOutgoingSet();

	SCM list = SCM_EOL;
	for (int i = oset.size()-1; i >= 0; i--)
	{
		if (oset[i]->getType() != t) continue;
		SCM smob = handle_to_scm(oset[i]);
		list = scm_cons (smob, list);
	}

	return list;
}

/* ============================================================== */
/**
 * Return the n'th atom of the outgoing set..
 */
SCM SchemeSmob::ss_outgoing_atom (SCM satom, SCM spos)
{
	Handle h = verify_handle(satom, "cog-outgoing-atom");
	size_t pos = verify_size(spos, "cog-outgoing-atom", 2);

	if (not h->isLink()) return SCM_EOL;

	const HandleSeq& oset = h->getOutgoingSet();
	if (oset.size() <= pos) return SCM_EOL;

	return handle_to_scm(oset[pos]);
}

/* ============================================================== */
/**
 * Convert the incoming set of an atom into a list; return the list.
 */
SCM SchemeSmob::ss_incoming_set (SCM satom)
{
	Handle h = verify_handle(satom, "cog-incoming-set");

	// This reverses the order of the incoming set, but so what ...
	SCM head = SCM_EOL;
	IncomingSet iset = h->getIncomingSet();
	for (const LinkPtr& l : iset)
	{
		SCM smob = handle_to_scm(l->getHandle());
		head = scm_cons(smob, head);
	}

	return head;
}

/* ============================================================== */
/**
 * Convert the incoming set of an atom into a list; return the list.
 */
SCM SchemeSmob::ss_incoming_by_type (SCM satom, SCM stype)
{
	Handle h = verify_handle(satom, "cog-incoming-by-type");
	Type t = verify_atom_type(stype, "cog-incoming-by-type", 2);

	HandleSeq iset;
	h->getIncomingSetByType(std::back_inserter(iset), t, false);
	SCM head = SCM_EOL;
	for (const Handle& ih : iset)
	{
		SCM smob = handle_to_scm(ih);
		head = scm_cons(smob, head);
	}

	return head;
}

/* ============================================================== */

/**
 * Apply proceedure proc to all atoms of type stype
 * If the proceedure returns something other than #f,
 * terminate the loop.
 */
SCM SchemeSmob::ss_map_type (SCM proc, SCM stype)
{
	Type t = verify_atom_type (stype, "cog-map-type");
	AtomSpace* atomspace = ss_get_env_as("cog-map-type");

	// Get all of the handles of the indicated type
	std::list<Handle> handle_set;
	atomspace->get_handles_by_type(back_inserter(handle_set), t, false);

	// Loop over all handles in the handle set.
	// Call proc on each handle, in turn.
	// Break out of the loop if proc returns anything other than #f
	std::list<Handle>::iterator i;
	for (i = handle_set.begin(); i != handle_set.end(); ++i) {
		Handle h = *i;
		SCM smob = handle_to_scm(h);
		SCM rc = scm_call_1(proc, smob);
		if (!scm_is_false(rc)) return rc;
	}

	return SCM_BOOL_F;
}

/* ============================================================== */

/**
 * Return a list of all of the atom types in the system.
 */
SCM SchemeSmob::ss_get_types (void)
{
	SCM list = SCM_EOL;

	Type t = classserver().getNumberOfClasses();
	while (1) {
		t--;
		const std::string &tname = classserver().getTypeName(t);
		SCM str = scm_from_utf8_string(tname.c_str());
		SCM sym = scm_string_to_symbol(str);
		list = scm_cons(sym, list);
		if (0 == t) break;
	}

	return list;
}

/**
 * Return a list of the subtypes of the indicated type
 */
SCM SchemeSmob::ss_get_subtypes (SCM stype)
{
	SCM list = SCM_EOL;

	Type t = verify_atom_type(stype, "cog-get-subtypes");
	std::vector<Type> subl;
	unsigned int ns = classserver().getChildren(t, std::back_inserter(subl));

	for (unsigned int i=0; i<ns; i++) {
		t = subl[i];
		const std::string &tname = classserver().getTypeName(t);
		SCM str = scm_from_utf8_string(tname.c_str());
		SCM sym = scm_string_to_symbol(str);
		list = scm_cons(sym, list);
	}

	return list;
}

/**
 * Return integer value corresponding to the string type name.
 */
SCM SchemeSmob::ss_get_type (SCM stype)
{
	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	static_assert(2 == sizeof(Type),
		"*** Code currently assumes types are shorts!  ***");

	if (scm_is_false(scm_string_p(stype)))
		scm_wrong_type_arg_msg("cog-type->int", 0, stype, "opencog atom type");

	const char * ct = scm_i_string_chars(stype);
	Type t = classserver().getType(ct);
	if (NOTYPE == t and strcmp(ct, "Notype"))
		scm_wrong_type_arg_msg("cog-type->int", 0, stype, "opencog atom type");

	return scm_from_ushort(t);
}

/**
 * Return true if stype is an atom type
 */
SCM SchemeSmob::ss_type_p (SCM stype)
{
	if (scm_is_integer(stype)) {
		Type t = scm_to_ushort(stype);
		if (classserver().isValue(t))
			return SCM_BOOL_T;
		return SCM_BOOL_F;
	}

	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	if (scm_is_false(scm_string_p(stype)))
		return SCM_BOOL_F;

	const char * ct = scm_i_string_chars(stype);
	Type t = classserver().getType(ct);

	if (NOTYPE == t) return SCM_BOOL_F;

	return SCM_BOOL_T;
}

/**
 * Return true if stype is a value type
 */
SCM SchemeSmob::ss_value_type_p (SCM stype)
{
	if (scm_is_integer(stype)) {
		Type t = scm_to_ushort(stype);
		if (classserver().isValue(t) and not classserver().isAtom(t))
			return SCM_BOOL_T;
		return SCM_BOOL_F;
	}

	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	if (scm_is_false(scm_string_p(stype)))
		return SCM_BOOL_F;

	const char * ct = scm_i_string_chars(stype);
	Type t = classserver().getType(ct);

	if (NOTYPE == t) return SCM_BOOL_F;
	if (classserver().isValue(t) and not classserver().isAtom(t))
		return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/**
 * Return true if stype is a node type
 */
SCM SchemeSmob::ss_node_type_p (SCM stype)
{
	if (scm_is_integer(stype)) {
		Type t = scm_to_ushort(stype);
		if (classserver().isNode(t))
			return SCM_BOOL_T;
		return SCM_BOOL_F;
	}

	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	if (scm_is_false(scm_string_p(stype)))
		return SCM_BOOL_F;

	const char * ct = scm_i_string_chars(stype);
	Type t = classserver().getType(ct);

	if (NOTYPE == t) return SCM_BOOL_F;
	if (classserver().isNode(t)) return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/**
 * Return true if stype is a link type
 */
SCM SchemeSmob::ss_link_type_p (SCM stype)
{
	if (scm_is_integer(stype)) {
		Type t = scm_to_ushort(stype);
		if (classserver().isLink(t))
			return SCM_BOOL_T;
		return SCM_BOOL_F;
	}

	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	if (scm_is_false(scm_string_p(stype)))
		return SCM_BOOL_F;

	const char * ct = scm_i_string_chars(stype);
	Type t = classserver().getType(ct);

	if (NOTYPE == t) return SCM_BOOL_F;
	if (classserver().isLink(t)) return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/**
 * Return true if a subtype
 */
SCM SchemeSmob::ss_subtype_p (SCM stype, SCM schild)
{
	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	if (scm_is_false(scm_string_p(stype)))
		return SCM_BOOL_F;

	const char * ct = scm_i_string_chars(stype);
	Type parent = classserver().getType(ct);

	if (NOTYPE == parent) return SCM_BOOL_F;

	// Now investigate the child ...
	if (scm_is_true(scm_symbol_p(schild)))
		schild = scm_symbol_to_string(schild);

	if (scm_is_false(scm_string_p(schild)))
		return SCM_BOOL_F;

	const char * cht = scm_i_string_chars(schild);
	Type child = classserver().getType(cht);

	if (NOTYPE == child) return SCM_BOOL_F;

	if (classserver().isA(child, parent)) return SCM_BOOL_T;

	return SCM_BOOL_F;
}

SCM SchemeSmob::ss_get_free_variables(SCM satom)
{
	Handle h = verify_handle(satom, "cog-free-variables");

	SCM list = SCM_EOL;
	for (const Handle& fv : get_free_variables(h))
		list = scm_cons(handle_to_scm(fv), list);

	return list;
}

/**
 * Return true if the atom is closed (has no variable)
 */
SCM SchemeSmob::ss_is_closed(SCM satom)
{
	Handle h = verify_handle(satom, "cog-closed?");
	return is_closed(h) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* ===================== END OF FILE ============================ */
