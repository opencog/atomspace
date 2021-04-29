/*
 * SchemeSmobNew.cc
 *
 * Scheme small objects (SMOBS) --creating new atoms -- for opencog.
 *
 * Copyright (c) 2008 Linas Vepstas <linas@linas.org>
 */

#include <vector>

#include <cstddef>
#include <libguile.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */
/**
 * Return a string holding the scheme representation of an opencog object.
 * This could be a Value including Atom, or an AtomSpace, or a
 * guile module created with the PrimitiveEnviron C++ module wrapper.
 *
 * Returned is a valid scheme expression that represents that object.
 */
std::string SchemeSmob::to_string(SCM node)
{
	if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, node))
		return misc_to_string(node);

	return "";
}

std::string SchemeSmob::protom_to_string(SCM node)
{
	ValuePtr pa(scm_to_protom(node));
	if (nullptr == pa) return "#<Invalid handle>";

	// Need to have a newline printed; otherewise
	// cog-value->list prints badly-formatted grunge.
	if (not pa->is_atom())
		return pa->to_string() + "\n";

	// Avoid printing atoms that are not in any atomspace.
	// Doing so, and more generally, keeping these around
	// just leads to confusion on the part of the user.
	// The current scheme bindings were designed to assume
	// that atoms are always in some atomspace, and having
	// free-floating atoms that aren't anywhere is not helpful
	// for anyone, as far as I can tell.
	// See issue opencog/atomspace#127 for one such report.
	Handle h(HandleCast(pa));
	if (nullptr == h->getAtomSpace())
	{
		h = Handle::UNDEFINED;
		*((Handle *) SCM_SMOB_DATA(node)) = Handle::UNDEFINED;
		scm_remember_upto_here_1(node);
		return "#<Invalid handle>";
	}

	// Need to have a newline printed; otherewise cog-value->list
	// prints badly-formatted grunge.
	return h->to_short_string() + "\n";
}

/* ============================================================== */
/**
 * Wrapper -- given a handle, return the corresponding scheme object.
 * Note that smobs hold the integer value of the handle, and not the
 * C++ handle object itself.
 */

SCM SchemeSmob::handle_to_scm (const Handle& h)
{
	return protom_to_scm(AtomCast(h));
}

SCM SchemeSmob::protom_to_scm (const ValuePtr& pa)
{
	if (nullptr == pa) return SCM_BOOL_F;

	// Use new so that the smart pointer increments!
	ValuePtr* pap = new ValuePtr(pa);
	scm_gc_register_allocation(sizeof(pa));

	SCM smob;
	SCM_NEWSMOB (smob, cog_misc_tag, pap);
	SCM_SET_SMOB_FLAGS(smob, COG_PROTOM);
	return smob;
}

ValuePtr SchemeSmob::scm_to_protom (SCM sh)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sh))
#ifdef RAINY_DAY_PROJECT
	{
		if (scm_is_false(sh))
			return ValueCast(TruthValue::FALSE_TV());
		return ValueCast(TruthValue::TRUE_TV());
	}
#else
		return nullptr;
#endif

	scm_t_bits misctype = SCM_SMOB_FLAGS(sh);
	if (COG_PROTOM != misctype) // Should this be a wrong-type-arg?
		return nullptr;

	ValuePtr pv(*((ValuePtr *) SCM_SMOB_DATA(sh)));
	scm_remember_upto_here_1(sh);
	return pv;
}

Handle SchemeSmob::scm_to_handle (SCM sh)
{
	ValuePtr pa(scm_to_protom(sh));
	if (nullptr == pa)
		return Handle::UNDEFINED;

	if (not pa->is_atom())
		return Handle::UNDEFINED;

	Handle h(HandleCast(pa));

	// Clobber deleted atoms (i.e. atoms that are no longer
	// in any atomspace). The current guile interface was
	// designed so that all atoms are in atomspaces, and any
	// exceptions to this assumption leads to confusion and
	// unexpected behavior -- i.e. leads to bugs.
	if (nullptr == h->getAtomSpace())
	{
		*((Handle *) SCM_SMOB_DATA(sh)) = Handle::UNDEFINED;
		scm_remember_upto_here_1(sh);
		return Handle::UNDEFINED;
	}
	scm_remember_upto_here_1(sh);
	return h;
}

/* ============================================================== */
/**
 * Return hash of atom
 */
SCM SchemeSmob::ss_handle (SCM satom)
{
	Handle h(scm_to_handle(satom));
	if (nullptr == h)
		scm_wrong_type_arg_msg("cog-handle", 1, satom, "opencog atom");

	return scm_from_ulong(h.value());
}

/** Return #t if left < right, else return #f. */
SCM SchemeSmob::ss_atom_less_p (SCM sleft, SCM sright)
{
	Handle hleft(scm_to_handle(sleft));
	Handle hright(scm_to_handle(sright));
	if (nullptr == hleft)
		scm_wrong_type_arg_msg("cog-atom-less?", 1, sleft, "opencog atom");
	if (nullptr == hright)
		scm_wrong_type_arg_msg("cog-atom-less?", 1, sright, "opencog atom");

	if (hleft < hright) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/* ============================================================== */
/** Return true if s is an atom. Invalid handles are not atoms. */

SCM SchemeSmob::ss_atom_p (SCM s)
{
	if (nullptr == scm_to_handle(s))
		return SCM_BOOL_F;

	return SCM_BOOL_T;
}

/* ============================================================== */
/** Return true if s is a node */

SCM SchemeSmob::ss_node_p (SCM s)
{
	Handle h(scm_to_handle(s));
	if (nullptr == h)
		return SCM_BOOL_F;

	if (h->is_node()) return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/* ============================================================== */
/** Return true if s is a link */

SCM SchemeSmob::ss_link_p (SCM s)
{
	Handle h(scm_to_handle(s));
	if (nullptr == h)
		return SCM_BOOL_F;

	if (h->is_link()) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/* ============================================================== */
/**
 * Check that the argument is the string or symbol name of an atom,
 * else throw errors.
 * Return the atom type.
 */
Type SchemeSmob::verify_type (SCM stype, const char *subrname, int pos)
{
	if (scm_is_integer(stype))
		return scm_to_ushort(stype);

	if (scm_is_true(scm_symbol_p(stype)))
		stype = scm_symbol_to_string(stype);

	if (scm_is_false(scm_string_p(stype)))
		scm_wrong_type_arg_msg(subrname, pos, stype, "name of opencog atom type");

	const char * ct = scm_i_string_chars(stype);
	Type t = nameserver().getType(ct);

	// Make sure that the type is good
	if (NOTYPE == t)
		scm_wrong_type_arg_msg(subrname, pos, stype, "name of opencog atom type");

	return t;
}


/**
 * Check that the argument is an int, else throw errors.
 * Return the int.
 */
int SchemeSmob::verify_int (SCM sint, const char *subrname,
                            int pos, const char * msg)
{
	if (scm_is_false(scm_integer_p(sint)))
		scm_wrong_type_arg_msg(subrname, pos, sint, msg);

	return scm_to_int(sint);
}

/**
 * Check that the argument is a size_t, else throw errors.
 */
size_t SchemeSmob::verify_size_t (SCM ssizet, const char *subrname,
                                  int pos, const char * msg)
{
	if (scm_is_false(scm_integer_p(ssizet)))
		scm_wrong_type_arg_msg(subrname, pos, ssizet, msg);

	return scm_to_size_t(ssizet);
}

/**
 * Check that the argument is convertible to a real, else throw errors.
 * Return as a float.
 */
double SchemeSmob::verify_real (SCM sreal, const char *subrname,
                                int pos, const char * msg)
{
	if (scm_is_false(scm_real_p(sreal)))
		scm_wrong_type_arg_msg(subrname, pos, sreal, msg);

	return scm_to_double(sreal);
}

/**
 * Check that the argument is a string, else throw errors.
 * Return the string, in C.
 */
std::string SchemeSmob::verify_string (SCM sname, const char *subrname,
                                       int pos, const char * msg)
{
	if (scm_is_false(scm_string_p(sname)))
		scm_wrong_type_arg_msg(subrname, pos, sname, msg);

	char * cname = scm_to_utf8_string(sname);
	std::string name(cname);
	free(cname);
	return name;
}

/* ============================================================== */
/**
 * Copy an existing atom into a new atomspace.
 */
SCM SchemeSmob::ss_new_atom (SCM satom, SCM kv_pairs)
{
	Handle h = verify_handle(satom, "cog-new-atom");

	AtomSpace* atomspace = get_as_from_list(kv_pairs);
	if (nullptr == atomspace) atomspace = ss_get_env_as("cog-new-atom");

	try
	{
		return handle_to_scm(atomspace->add_atom(h));
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-new-atom", scm_cons(satom, kv_pairs));
	}

	scm_remember_upto_here_1(kv_pairs);
	return SCM_EOL;
}

/**
 * Return the indicated atom, if a version of it exists in this
 * atomspace; else return nil if it does not exist.
 */
SCM SchemeSmob::ss_atom (SCM satom, SCM kv_pairs)
{
	Handle h = verify_handle(satom, "cog-atom");

	AtomSpace* atomspace = get_as_from_list(kv_pairs);
	if (nullptr == atomspace) atomspace = ss_get_env_as("cog-atom");

	try
	{
		return handle_to_scm(atomspace->get_atom(h));
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-atom", scm_cons(satom, kv_pairs));
	}

	scm_remember_upto_here_1(kv_pairs);
	return SCM_EOL;
}

/* ============================================================== */
/**
 * Create a new node, of named type stype, and string name sname
 */
SCM SchemeSmob::ss_new_node (SCM stype, SCM sname, SCM kv_pairs)
{
	Type t = verify_type(stype, "cog-new-node", 1);

	// Special case handling for NumberNode (and TimeNode, etc.)
	std::string name;
	if (nameserver().isA(t, NUMBER_NODE)) {
		std::vector<double> vec;
		SCM slist = SCM_EOL;
		if (scm_is_number(sname))
		{
			slist = scm_cons(sname, kv_pairs);
		}
		else
		if (scm_is_true(scm_list_p(sname)))
		{
			slist = sname;
		}
		if (not scm_is_null(slist))
		{
			while (scm_is_pair(slist))
			{
				SCM sval = SCM_CAR(slist);
				if (scm_is_number(sval))
					vec.push_back(scm_to_double(sval));
				slist = SCM_CDR(slist);
			}
			name = NumberNode::vector_to_plain(vec);
		}
	}
	else
	// Allow symbols as well as strings for atom types.
	if (nameserver().isA(t, TYPE_NODE) and scm_is_symbol(sname)) {
		sname = scm_symbol_to_string(sname);
	}

	// Haven't set the string yet...
	if (0 == name.size())
		name = verify_string (sname, "cog-new-node", 2,
			"string name for the node");

	AtomSpace* atomspace = get_as_from_list(kv_pairs);
	if (nullptr == atomspace) atomspace = ss_get_env_as("cog-new-node");

	try
	{
		// Now, create the actual node... in the actual atom space.
		Handle h(atomspace->add_node(t, std::move(name)));

		if (h)
		{
			const TruthValuePtr tv(get_tv_from_list(kv_pairs));
			if (tv) h = atomspace->set_truthvalue(h, tv);
		}

		return handle_to_scm(h);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-new-node", scm_cons(sname, kv_pairs));
	}

	scm_remember_upto_here_1(kv_pairs);
	return SCM_EOL;
}

/**
 * Return the indicated node, of named type stype, and string name sname
 * if it exists; else return nil if it does not exist.
 * If the node exists, *and* a truth value was specified, then change
 * the truth value.
 */
SCM SchemeSmob::ss_node (SCM stype, SCM sname, SCM kv_pairs)
{
	Type t = verify_type(stype, "cog-node", 1);
	std::string name = verify_string (sname, "cog-node", 2,
									"string name for the node");

	AtomSpace* atomspace = get_as_from_list(kv_pairs);
	if (nullptr == atomspace) atomspace = ss_get_env_as("cog-node");

	// Now, look for the actual node... in the actual atom space.
	Handle h(atomspace->get_node(t, std::string(name)));
	if (nullptr == h) return SCM_EOL;

	// If there was a truth value, change it.
	const TruthValuePtr tv(get_tv_from_list(kv_pairs));
	if (tv) h = atomspace->set_truthvalue(h, tv);

	scm_remember_upto_here_1(kv_pairs);
	return handle_to_scm (h);
}

/* ============================================================== */

/**
 * Convert argument into a list of handles.
 */
HandleSeq
SchemeSmob::verify_handle_list (SCM satom_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list. Allow null list
	// (which is rather unusual, but legit.  Allow embedded nulls
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(satom_list) and !scm_is_null(satom_list))
		scm_wrong_type_arg_msg(subrname, pos, satom_list, "a list of atoms");

	HandleSeq outgoing_set;
	SCM sl = satom_list;
	pos = 2;
	while (scm_is_pair(sl)) {
		SCM satom = SCM_CAR(sl);

		// Verify that the contents of the list are actual atoms.
		Handle h(scm_to_handle(satom));
		if (h) {
			outgoing_set.emplace_back(h);
		}
		else if (scm_is_pair(satom) and !scm_is_null(satom_list)) {
			// Allow lists to be specified: e.g.
			// (cog-new-link 'ListLink (list x y z))
			// Do this via a recursive call, flattening nested lists
			// as we go along.
			const HandleSeq &oset =
				verify_handle_list(satom, subrname, pos);
			HandleSeq::const_iterator it;
			for (it = oset.begin(); it != oset.end(); ++it) {
				outgoing_set.emplace_back(*it);
			}
		}
		else if (scm_is_null(satom)) {
			// No-op, just ignore.
		}
		else {
			// Its legit to have embedded truth values, just skip them.
			if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, satom)) {
				// If its not an atom, and its not a truth value, and
				// its not an attention value, and its not an atomspace,
				// then whatever it is, its bad.
				scm_wrong_type_arg_msg(subrname, pos, satom, "opencog atom");
			}
		}
		sl = SCM_CDR(sl);
		pos++;
	}

	return outgoing_set;
}

/**
 * Create a new link, of named type stype, holding the indicated atom list
 */
SCM SchemeSmob::ss_new_link (SCM stype, SCM satom_list)
{
	Type t = verify_type(stype, "cog-new-link", 1);

	HandleSeq outgoing_set;
	outgoing_set = verify_handle_list(satom_list, "cog-new-link", 2);

	AtomSpace* atomspace = get_as_from_list(satom_list);
	if (nullptr == atomspace) atomspace = ss_get_env_as("cog-new-link");

	try
	{
		// Now, create the actual link... in the actual atom space.
		Handle h(atomspace->add_link(t, std::move(outgoing_set)));

		// Fish out a truth value, if its there.
		if (h)
		{
			const TruthValuePtr tv(get_tv_from_list(satom_list));
			if (tv) h = atomspace->set_truthvalue(h, tv);
		}

		return handle_to_scm(h);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-new-link", satom_list);
	}
	scm_remember_upto_here_1(satom_list);
	return SCM_EOL;
}

/**
 * Return the indicated link, of named type stype, holding the
 * indicated atom list, if it exists; else return nil if
 * it does not exist.
 */
SCM SchemeSmob::ss_link (SCM stype, SCM satom_list)
{
	Type t = verify_type(stype, "cog-link", 1);

	HandleSeq outgoing_set;
	outgoing_set = verify_handle_list (satom_list, "cog-link", 2);

	AtomSpace* atomspace = get_as_from_list(satom_list);
	if (nullptr == atomspace) atomspace = ss_get_env_as("cog-link");

	// Now, look to find the actual link... in the actual atom space.
	Handle h(atomspace->get_link(t, std::move(outgoing_set)));
	if (nullptr == h) return SCM_EOL;

	// If there was a truth value, change it.
	const TruthValuePtr tv(get_tv_from_list(satom_list));
	if (tv) h = atomspace->set_truthvalue(h, tv);

	scm_remember_upto_here_1(satom_list);
	return handle_to_scm (h);
}

/* ============================================================== */
/**
 * Extract the atom from the atomspace, but only if it has no incoming
 * links. Return `SCM_BOOL_T` if the atom was successfully extracted,
 * else return `SCM_BOOL_F`.  This does NOT remove the atom from any
 * attached backing store/persistant storage, only from the (local,
 * in-RAM) atomspace.
 */
SCM SchemeSmob::ss_extract (SCM satom, SCM kv_pairs)
{
	Handle h = verify_handle(satom, "cog-extract!");

	// The extract will fail/log warning if the incoming set isn't null.
	if (h->getIncomingSetSize() > 0) return SCM_BOOL_F;

	AtomSpace* atomspace = get_as_from_list(kv_pairs);
	if (NULL == atomspace) atomspace = ss_get_env_as("cog-extract!");

	// AtomSpace::extract_atom() returns true if atom was extracted,
	// else returns false
	bool rc = atomspace->extract_atom(h, false);

	// Clobber the handle, too.
	*((Handle *) SCM_SMOB_DATA(satom)) = Handle::UNDEFINED;
	scm_remember_upto_here_1(satom);

	// rc should always be true at this point ...
	if (rc) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/* ============================================================== */
/**
 * Extract the atom, and everything pointing to it.
 * This does NOT remove the atom from any attached backing store, only
 * from the atomspace.
 */
SCM SchemeSmob::ss_extract_recursive (SCM satom, SCM kv_pairs)
{
	Handle h = verify_handle(satom, "cog-extract-recursive!");

	AtomSpace* atomspace = get_as_from_list(kv_pairs);
	if (NULL == atomspace) atomspace = ss_get_env_as("cog-extract-recursive!");

	bool rc = atomspace->extract_atom(h, true);

	// Clobber the handle, too.
	*((Handle *) SCM_SMOB_DATA(satom)) = Handle::UNDEFINED;
	scm_remember_upto_here_1(satom);

	if (rc) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

/* ===================== END OF FILE ============================ */
