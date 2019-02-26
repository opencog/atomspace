/*
 * BindLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/query/DefaultImplicator.h>

#include "BindLink.h"

using namespace opencog;

void BindLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, BIND_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a BindLink, got %s", tname.c_str());
	}

	extract_variables(_outgoing);
	unbundle_clauses(_body);
	common_init();
	setup_components();
	_pat.redex_name = "anonymous BindLink";
}

BindLink::BindLink(const Handle& vardecl,
                   const Handle& body,
                   const Handle& rewrite)
	: BindLink(HandleSeq{vardecl, body, rewrite})
{}

BindLink::BindLink(const Handle& body, const Handle& rewrite)
	: BindLink(HandleSeq{body, rewrite})
{}

BindLink::BindLink(const HandleSeq& hseq, Type t)
	: PatternLink(hseq, t)
{
	init();
}

BindLink::BindLink(const Link &l)
	: PatternLink(l)
{
	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
/// On top of that initialize _body and _implicand with the
/// clauses and the rewrite rule.
///
void BindLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = oset.size();
	if (sz < 2 or 3 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at most two, got %d", sz);

	// If the outgoing set size is two, then there are no variable
	// declarations; extract all free variables.
	if (2 == sz)
	{
		_body = oset[0];
		_implicand = oset[1];
		_varlist.find_variables(oset[0]);
		return;
	}

	// If we are here, then the first outgoing set member should be
	// a variable declaration.
	_vardecl = oset[0];
	_body = oset[1];
	_implicand = oset[2];

	// Initialize _varlist with the scoped variables
	init_scoped_variables(oset[0]);
}

/* ================================================================= */
// Cache of the most results obtained from the most recent run
// of the pattern matcher.

static const Handle& rewrite_key(void)
{
	static Handle rk(createNode(PREDICATE_NODE, "*-PatternRewriteKey-*"));
	return rk;
}

/// Store a cache of the most recent pattern rewrite as a value,
/// obtainable via a "well-known" key: "*-PatternRewriteKey-*"
void BindLink::set_rewrite(const Handle& rewr)
{
	setValue(rewrite_key(), rewr);
}

/// Return the cached value of the most recent rewrite.
Handle BindLink::get_rewrite(void) const
{
	return HandleCast(getValue(rewrite_key()));
}

/* ================================================================= */
/* ================================================================= */

/**
 * Simplified utility
 *
 * The `do_conn_check` flag stands for "do connectivity check"; if the
 * flag is set, and the pattern is disconnected, then an error will be
 * thrown. The URE explicitly allows disconnected graphs.
 *
 * Set the default to always allow disconnected graphs. This will
 * get naive users into trouble, but there are legit uses, not just
 * in the URE, for doing disconnected searches.
 */
static Handle do_imply(AtomSpace* as,
                       const Handle& hbindlink,
                       Implicator& impl,
                       bool do_conn_check=false)
{
	BindLinkPtr bl(BindLinkCast(hbindlink));

	impl.implicand = bl->get_implicand();

	bl->imply(impl, do_conn_check);

	// If we got a non-empty answer, just return it.
	if (0 < impl.get_result_list().size())
	{
		// The result_list contains a list of the grounded expressions.
		// (The order of the list has no significance, so it's really a set.)
		// Put the set into a SetLink, cache it, and return that.
		Handle rewr(createLink(impl.get_result_list(), SET_LINK));

#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
		// Shoot. XXX FIXME. Most of the unit tests require that the atom
		// that we return is in the atomspace. But it would be nice if we
		// could defer this indefinitely, until its really needed.
		rewr = as->add_atom(rewr);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */

		bl->set_rewrite(rewr);
		return rewr;
	}

	// If we are here, then there were zero matches.
	//
	// There are certain useful queries, where the goal of the query
	// is to determine that some clause or set of clauses are absent
	// from the AtomSpace. If the clauses are jointly not found, after
	// a full and exhaustive search, then we want to run the implicator,
	// and perform some action. Easier said than done, this code is
	// currently a bit of a hack. It seems to work, per the AbsentUTest
	// but is perhaps a bit fragile in its assumptions.
	//
	// Theoretical background: the atomspace can be thought of as a
	// Kripke frame: it holds everything we know "right now". The
	// AbsentLink is a check for what we don't know, right now.
	const Pattern& pat = bl->get_pattern();
	DefaultPatternMatchCB* intu =
		dynamic_cast<DefaultPatternMatchCB*>(&impl);
	if (0 == pat.mandatory.size() and 0 < pat.optionals.size()
	    and not intu->optionals_present())
	{
		Handle h(HandleCast(impl.inst.execute(impl.implicand, true)));
		impl.insert_result(h);
	}

	// Create a set holding all results of the implication, and cache it.
	Handle rewr(createLink(impl.get_result_list(), SET_LINK));

#ifdef PLACE_RESULTS_IN_ATOMSPACE
	// Shoot. XXX FIXME. Most of the unit tests require that the atom
	// that we return is in the atomspace. But it would be nice if we
	// could defer this indefinitely, until its really needed.
	rewr = as->add_atom(rewr);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */
	bl->set_rewrite(rewr);

	return rewr;
}

/* ================================================================= */

/**
 * Evaluate a pattern and rewrite rule embedded in a BindLink
 *
 * Use the default implicator to find pattern-matches. Associated truth
 * values are completely ignored during pattern matching; if a set of
 * atoms that could be a ground are found in the atomspace, then they
 * will be reported.
 *
 * See the do_imply function documentation for details.
 */
ValuePtr BindLink::execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;

	DefaultImplicator impl(as);
	impl.max_results = SIZE_MAX;
	// Now perform the search.
	return do_imply(as, get_handle(), impl);
}

DEFINE_LINK_FACTORY(BindLink, BIND_LINK)

/* ===================== END OF FILE ===================== */
