/*
 * SchemeSmobPrint.cc
 *
 * Scheme small objects (SMOBS) printing.
 *
 * Copyright (c) 2008,2009,2014,2020 Linas Vepstas <linas@linas.org>
 */

#include <libguile.h>

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/// If server_mode is true, then do things that a server would want;
/// otherwise, behave in a human-friendly interactive way.  Currently,
/// server-mode enables printing of 16-decimal-place truth values.
bool SchemeSmob::server_mode = false;

SCM SchemeSmob::ss_set_server_mode(SCM boo)
{
	bool old_mode = server_mode;
	server_mode = (SCM_BOOL_F != boo);
	return old_mode ? SCM_BOOL_T : SCM_BOOL_F;
}

/* ============================================================== */

static std::string server_prt_node(const Handle& h)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type())
		+ " \"" + h->get_name() + "\")";
	return txt;
}

static std::string server_prt_atom(const Handle&);

static std::string server_prt_link(const Handle& h)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += server_prt_atom(ho);
	txt += ")";
	return txt;
}

static std::string server_prt_atom(const Handle& h)
{
	if (ATOM_SPACE == h->get_type()) return h->to_string();
	if (h->is_node()) return server_prt_node(h);
	return server_prt_link(h);
}

std::string SchemeSmob::protom_to_server_string(SCM node)
{
	ValuePtr pa(scm_to_protom(node));
	if (nullptr == pa) return "#<Invalid handle>";

	if (not pa->is_atom())
	{
		// Print high-precision simple truth values.
		if (nameserver().isA(pa->get_type(), FLOAT_VALUE))
		{
			// The FloatValue to_string() print prints out a
			// high-precision form of the value, as compared
			// to SimpleTruthValue, which only prints 6 digits
			// and breaks distributed-storage unit tests.
			FloatValuePtr fv(FloatValueCast(pa));
			return fv->FloatValue::to_string();
		}
		return pa->to_string();
	}

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

	return server_prt_atom(h);
}

/* ============================================================== */

std::string SchemeSmob::misc_to_string(SCM node)
{
	scm_t_bits misctype = SCM_SMOB_FLAGS(node);
	switch (misctype)
	{
		case COG_LOGGER:
		{
			std::string str(logger_to_string((Logger *) SCM_SMOB_DATA(node)));
			scm_remember_upto_here_1(node);
			return str;
		}
		case COG_PROTOM:
			if (server_mode)
				return protom_to_server_string(node);
			return protom_to_string(node);

		case COG_EXTEND:
		{
			// return "#<opencog extension>\n";
			// Hmm. Is this really the right thing to return ?? I'm not sure ..
			PrimitiveEnviron * pe = (PrimitiveEnviron *) SCM_SMOB_DATA(node);
			std::string str(pe->get_name());
			scm_remember_upto_here_1(node);
			return str;
		}
		default:
			return "#<unknown opencog type>\n";
	}
	return "";
}

int SchemeSmob::print_misc(SCM node, SCM port, scm_print_state * ps)
{
	std::string str = misc_to_string(node);

#if defined(HAVE_GUILE_2_2) || defined(HAVE_GUILE3)

	// Deal with a regression in guile-2.1.x See bug report
	// https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25397
	scm_display (scm_from_utf8_string (str.c_str()), port);
#else
	scm_puts (str.c_str(), port);
#endif // HAVE_GUILE_2_2

	return 1; //non-zero means success
}

/* ===================== END OF FILE ============================ */
