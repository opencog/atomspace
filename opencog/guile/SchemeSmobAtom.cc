/*
 * SchemeSmobAtom.c
 *
 * Scheme small objects (SMOBS) for opencog atom properties
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linas@linas.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <vector>

#include <cstddef>
// #include <iconv.h>
#include <libguile.h>
#include <wchar.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/guile/SchemeSmob.h>

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
	// values.  However, in the guile wrapper, we expect all
	// handles to be pointers to atoms; use verify_protom() instead,
	// if you just want Values.
	if (not (h->is_link() or h->is_node() or (ATOM_SPACE == h->get_type())))
		scm_wrong_type_arg_msg(subrname, pos, satom, "opencog atom");

	return h;
}

ValuePtr SchemeSmob::verify_protom (SCM satom, const char * subrname, int pos)
{
	ValuePtr pv(scm_to_protom(satom));
	if (nullptr == pv)
		scm_wrong_type_arg_msg(subrname, pos, satom, "opencog value");

	return pv;
}

/* ============================================================== */

// XXX FIXME. Work around the despicable, horrible guile UTF8 handling.
// I am flabbergasted. The guile people are smart, but they could not have
// possibly picked a crappier string handling design. Fuck me. See
// https://stackoverflow.com/questions/79329532/c-c-encode-binary-into-utf8
//
// So here's the deal. Atom names in Atomese are byte strings. Often,
// but not always UTF-8. The C++ API allows any byte string at all to be
// used. Ditto for python. However, guile wants strings to be utf8, and
// not bytevectors. So what should I do?
//
// Option 1)
// If the string contains non-utf8 chars, then create and return a guile
// bytevector. This has several problems.
//   1a) Guile does not have an API for converting a C string to a bytevector.
//   1b) Printing the result gives #vu8(102 111 111 98 97 114) instead of
//       something reasonable. ("foobar" in this case).
//
// Option 2)
// Escape the non-utf8 chars. But how? We could do what python does: turn
// them into "surrogate code points", in the range U+DC80 to U+DCFF.
// This is how python handles it, and being compatible with python would
// have been nice.
// Problem:
//   2a) Doing this causes scm_from_utf8_string() to throw an exception.
//
// Option 3)
// Escape the non-utf8 chars to unicode-private range U+E000 to U+F8FF
// Problems:
//   3a) Industry non-standard, idiosyncratic Atomese.
//   3b) If user passes this strings back as Atom names, then we must
//       scan **all** input strings and convert them back into byte
//       strings. This is a performance hit.
//
// Option 4)
// Escape the non-utf8 chars in some other way, e.g. by using ASCII
// shift-out and shift-in SO and SI 0xe and 0xf to demarcate begin and
// end, while everything in between is printed in hex.
// Problems:
//    Same as in 3)
//
// Option 5)
// Use iconv and discard the non-utf8 bytes.
// Problems:
//   5a) Dataloss.
//
// Option 6)
// Do nothing. Let the user take the exception. This sucks, because
// it forces the user to write some fairly complicated code to catch
// and handle the exceptions. Which is mostly pointless, because the
// user will almost never do anything with the string, anyway.
// Even worse: the actual exception thrown is a "compound exception",
// stunningly complicated to decode.
//
// Option 7)
// Randomly guess alternative locales. Yuck.
//
// Now, guile could have just accepted naked byte strings as strings,
// wasted zero CPU cycles in the process. If printing to some port
// happened to require conversion to some locale, that's when the
// conversion could have been done, and the exception thrown, if needed.
// Instead, guile is wasting CPU doing a pointless conversion, and
// forcing additional design complexity and performance hits on me.
// That's just wrong.

// Of all of above, pick option 3.
SCM SchemeSmob::convert_to_utf8 (void *data, SCM tag, SCM throw_args)
{
	char *inbuf = (char *) data;
	size_t ilen = strlen(inbuf);
	std::string out;
	mbstate_t ps;
	memset(&ps, 0, sizeof(ps));

	size_t i = 0;
	while (i<ilen)
	{
		int step = (int) mbrtowc(NULL, &inbuf[i], ilen-i+1, &ps);
		if (0 < step)
		{
			do
			{
				out.push_back(inbuf[i]); i++; step--;
			}
			while (0 < step);
			continue;
		}
		else if (0 == step)
			break;

		// Unicode Private Use Area, U+E000 - U+F8FF
		// Use the range U+E000 to U+E0FF
		out.push_back(0xee);
		unsigned char c = inbuf[i];
		if (c < 0x40) { out.push_back(0x80); out.push_back(0x80 + c); }
		else if (c < 0x80) { out.push_back(0x81); out.push_back(0x40 + c); }
		else if (c < 0xc0) { out.push_back(0x82); out.push_back(c); }
		else { out.push_back(0x83); out.push_back(c - 0x40); }

		// Note that Microsoft NTFS uses U+F000 plus ASCII codepoint for
		// encoding invalid chars in NTFS filenames. So using this would
		// lead to collisions and unexpected behavior.
#if 0
		// Use the range U+F800 to U+F8FF
		out.push_back(0xef);
		unsigned char c = inbuf[i];
		if (c < 0x40) { out.push_back(0xa0); out.push_back(0x80 + c); }
		else if (c < 0x80) { out.push_back(0xa1); out.push_back(0x40 + c); }
		else if (c < 0xc0) { out.push_back(0xa2); out.push_back(c); }
		else { out.push_back(0xa3); out.push_back(c - 0x40); }
#endif
		i++;
		memset(&ps, 0, sizeof(ps));
	}

	return scm_from_utf8_string(out.c_str());

#if CLOBBER_WITH_ICONV
	// Just clobber stuff. Ick.  This is option 5) above.
	static iconv_t cd = iconv_open("UTF-8//IGNORE", "UTF-8");
	char *inbuf = (char *) data;
	size_t ilen = strlen(inbuf);
	size_t olen = ilen+1;
	char *wbuf = (char *) malloc(olen);
	char *obuf = wbuf;
	iconv(cd, &inbuf, &ilen, &obuf, &olen);
	obuf[olen] = 0x0;
	printf("XXX FIXME Bad string %s\nconverted to %s\n", (char *) data, wbuf);
	SCM str = scm_from_utf8_string(obuf);
	free(wbuf);
	return str;
#endif
}

/**
 * Return the string name of the atom
 */
SCM SchemeSmob::ss_name (SCM satom)
{
	std::string name;
	Handle h = verify_handle(satom, "cog-name");
	if (h->is_node()) name = h->get_name();

#ifdef PLAIN_STRINGS
	SCM str = scm_from_utf8_string(name.c_str());
#else
	SCM str = scm_c_catch(SCM_BOOL_T,
		(scm_t_catch_body) scm_from_utf8_string, (void *) name.c_str(),
		SchemeSmob::convert_to_utf8, (void *) name.c_str(), NULL, NULL);
#endif
	return str;
}

/**
 * Return the number of the NumberNode
 */
SCM SchemeSmob::ss_number (SCM satom)
{
	Handle h = verify_handle(satom, "cog-number");

	NumberNodePtr nn(NumberNodeCast(h));
	// Faster than saying if (not nameserver().isA(h->get_type(), NUMBER_NODE))
	if (nullptr == nn)
		scm_wrong_type_arg_msg("cog-number", 0, satom, "NumberNode");
	SCM num = scm_from_double(nn->get_value());
	return num;
}

SCM SchemeSmob::ss_arity (SCM svalue)
{
	ValuePtr pa(verify_protom(svalue, "cog-arity"));
	return scm_from_size_t(pa->size());
}

/* ============================================================== */
/* Truth value setters/getters */

SCM SchemeSmob::ss_tv (SCM satom)
{
	Handle h = verify_handle(satom, "cog-tv");
	return protom_to_scm(ValueCast(h->getTruthValue()));
}

SCM SchemeSmob::ss_set_tv (SCM satom, SCM stv)
{
	Handle h = verify_handle(satom, "cog-set-tv!");
	TruthValuePtr tv = verify_tv(stv, "cog-set-tv!", 2);
	scm_remember_upto_here_1(stv);

	const AtomSpacePtr& asp = ss_get_env_as("cog-set-tv!");
	try
	{
		Handle newh = asp->set_truthvalue(h, tv);

		if (h == newh) return satom;
		return handle_to_scm(newh);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-set-tv!", satom);
	}
}

/// Atomic increment the count, keeping mean and confidence as-is.
/// Converts existing truth value to a CountTruthValue.
SCM SchemeSmob::ss_inc_count (SCM satom, SCM scnt)
{
	Handle h = verify_handle(satom, "cog-inc-count!");
	double cnt = verify_real(scnt, "cog-inc-count!", 2);

	const AtomSpacePtr& asp = ss_get_env_as("cog-inc-count!");
	Handle ha(asp->increment_countTV(h, cnt));
	if (ha == h)
		return satom;
	return handle_to_scm(ha);
}

/* ============================================================== */
/// Atomic increment the count of some generic FloatValue.
/// Just like ss_inc_count but generic.
/// key == key for value
/// cnt == how much to increment
/// ref == list-ref, which location to increment.
SCM SchemeSmob::ss_inc_value (SCM satom, SCM skey, SCM scnt, SCM sref)
{
	Handle h = verify_handle(satom, "cog-inc-value!");
	Handle key = verify_handle(skey, "cog-inc-value!", 2);
	double cnt = verify_real(scnt, "cog-inc-value!", 3);
	size_t ref = verify_size_t(sref, "cog-inc-value!", 4);

	const AtomSpacePtr& asp = ss_get_env_as("cog-inc-value!");
	Handle ha(asp->increment_count(h, key, ref, cnt));
	if (ha == h)
		return satom;
	return handle_to_scm(ha);
}

/* ============================================================== */

/// Generic atomic read-modify-write update
SCM SchemeSmob::ss_update_value (SCM satom, SCM skey, SCM sdelta)
{
	Handle h = verify_handle(satom, "cog-update-value!");
	Handle key = verify_handle(skey, "cog-update-value!", 2);
	ValuePtr vp = verify_protom(sdelta, "cog-update-value!", 3);

	// Only FloatValues are supported at this time.
	if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
	{
		static std::exception ex = RuntimeException(TRACE_INFO,
			"Expecting a FloatValue!");
		throw_exception(ex, "cog-update-value!", sdelta);
	}

	FloatValuePtr fvp = FloatValueCast(vp);

	const AtomSpacePtr& asp = ss_get_env_as("cog-update-value!");
	Handle ha(asp->increment_count(h, key, fvp->value()));
	if (ha == h)
		return satom;
	return handle_to_scm(ha);
}

/* ============================================================== */
/**
 * Convert the outgoing set of an atom into a list; return the list.
 */
SCM SchemeSmob::ss_outgoing_set (SCM satom)
{
	Handle h = verify_handle(satom, "cog-outgoing-set");

	if (not h->is_link()) return SCM_EOL;

	const HandleSeq& oset = h->getOutgoingSet();

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
 * Convert the outgoing set of an atom into a list;
 * filter-accept only type, and return the list.
 */
SCM SchemeSmob::ss_outgoing_by_type (SCM satom, SCM stype)
{
	Handle h = verify_handle(satom, "cog-outgoing-by-type");
	Type t = verify_type(stype, "cog-outgoing-by-type", 2);

	if (not h->is_link()) return SCM_EOL;

	const HandleSeq& oset = h->getOutgoingSet();

	SCM list = SCM_EOL;
	for (size_t i = oset.size(); i > 0; i--)
	{
		if (oset[i-1]->get_type() != t) continue;
		SCM smob = handle_to_scm(oset[i-1]);
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
	size_t pos = verify_size_t(spos, "cog-outgoing-atom", 2);

	if (not h->is_link()) return SCM_EOL;

	const HandleSeq& oset = h->getOutgoingSet();
	if (oset.size() <= pos) return SCM_EOL;

	return handle_to_scm(oset[pos]);
}

/* ============================================================== */
/**
 * Convert the incoming set of an atom into a list; return the list.
 */
SCM SchemeSmob::ss_incoming_set (SCM satom, SCM aspace)
{
	Handle h = verify_handle(satom, "cog-incoming-set");

	const AtomSpacePtr& asg = ss_to_atomspace(aspace);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-incoming-set");

	// This reverses the order of the incoming set, but so what ...
	SCM head = SCM_EOL;
	IncomingSet iset = h->getIncomingSet(asp.get());
	for (const Handle& l : iset)
	{
		SCM smob = handle_to_scm(l);
		head = scm_cons(smob, head);
	}

	return head;
}

/* ============================================================== */
/**
 * Convert the incoming set of an atom into a list; return the list.
 */
SCM SchemeSmob::ss_incoming_by_type (SCM satom, SCM stype, SCM aspace)
{
	Handle h = verify_handle(satom, "cog-incoming-by-type");
	Type t = verify_type(stype, "cog-incoming-by-type", 2);

	const AtomSpacePtr& asg = ss_to_atomspace(aspace);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-incoming-by-type");

	IncomingSet iset = h->getIncomingSetByType(t, asp.get());
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
 * Return the length (size) of the incoming set of an atom.
 */
SCM SchemeSmob::ss_incoming_size (SCM satom, SCM aspace)
{
	Handle h = verify_handle(satom, "cog-incoming-size");
	const AtomSpacePtr& asg = ss_to_atomspace(aspace);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-incoming-size");

	size_t sz = h->getIncomingSetSize(asp.get());
	return scm_from_size_t(sz);
}

/* ============================================================== */
/**
 * Return the length (size) of the incoming set of type stype
 * of the atom.
 */
SCM SchemeSmob::ss_incoming_size_by_type (SCM satom, SCM stype, SCM aspace)
{
	Handle h = verify_handle(satom, "cog-incoming-size-by-type");
	Type t = verify_type(stype, "cog-incoming-size-by-type", 2);

	const AtomSpacePtr& asg = ss_to_atomspace(aspace);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-incoming-size-by-type");

	size_t sz = h->getIncomingSetSizeByType(t, asp.get());
	return scm_from_size_t(sz);
}

/* ============================================================== */

/**
 * Apply procedure proc to all atoms of type stype
 * If the procedure returns something other than #f,
 * terminate the loop.
 * The `aspace` argument is optional; use it if present, else not.
 */
SCM SchemeSmob::ss_map_type (SCM proc, SCM stype, SCM aspace)
{
	Type t = verify_type (stype, "cog-map-type");

	const AtomSpacePtr& asg = ss_to_atomspace(aspace);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-map-type");

	// Get all of the handles of the indicated type
	HandleSeq hset;
	asp->get_handles_by_type(hset, t);

	// Loop over all handles in the handle set.
	// Call proc on each handle, in turn.
	// Break out of the loop if proc returns anything other than #f
	for (const Handle& h : hset) {

		// In case h got removed from the atomspace between
		// get_handles_by_type call and now. This may happen either
		// externally or by proc itself (such as cog-extract-recursive)
		if (not h->getAtomSpace() and not (ATOM_SPACE == h->get_type()))
			continue;

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

	Type t = nameserver().getNumberOfClasses();
	while (1) {
		t--;
		if (nameserver().isDefined(t))
		{
			const std::string &tname = nameserver().getTypeName(t);
			SCM str = scm_from_utf8_string(tname.c_str());
			SCM sym = scm_string_to_symbol(str);
			list = scm_cons(sym, list);
		}
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

	Type t = verify_type(stype, "cog-get-subtypes");
	std::vector<Type> subl;
	unsigned int ns = nameserver().getChildren(t, std::back_inserter(subl));

	for (unsigned int i=0; i<ns; i++) {
		t = subl[i];
		const std::string &tname = nameserver().getTypeName(t);
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
	Type t = nameserver().getType(ct);
	if (NOTYPE == t and strcmp(ct, "Notype"))
		scm_wrong_type_arg_msg("cog-type->int", 0, stype, "opencog atom type");

	return scm_from_ushort(t);
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
	Type parent = nameserver().getType(ct);

	if (NOTYPE == parent) return SCM_BOOL_F;

	// Now investigate the child ...
	if (scm_is_true(scm_symbol_p(schild)))
		schild = scm_symbol_to_string(schild);

	if (scm_is_false(scm_string_p(schild)))
		return SCM_BOOL_F;

	const char * cht = scm_i_string_chars(schild);
	Type child = nameserver().getType(cht);

	if (NOTYPE == child) return SCM_BOOL_F;

	if (nameserver().isA(child, parent)) return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/**
 * Return a count of the number of atoms of the indicated type.
 * The aspace argument is optional.
 */
SCM SchemeSmob::ss_count (SCM stype, SCM aspace)
{
	Type t = verify_type(stype, "cog-count-atoms");

	const AtomSpacePtr& asg = ss_to_atomspace(aspace);
	const AtomSpacePtr& asp = asg ? asg :
		ss_get_env_as("cog-count-atoms");

	size_t cnt = asp->get_num_atoms_of_type(t);
	return scm_from_size_t(cnt);
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
