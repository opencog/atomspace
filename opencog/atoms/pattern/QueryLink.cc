/*
 * QueryLink.cc
 *
 * Copyright (C) 2009, 2014, 2015, 2019 Linas Vepstas
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
#include <opencog/query/DefaultImplicator.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "QueryLink.h"

using namespace opencog;

void QueryLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, QUERY_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a QueryLink, got %s", tname.c_str());
	}

	extract_variables(_outgoing);
	unbundle_clauses(_body);
	common_init();
	setup_components();
	_pat.redex_name = "anonymous QueryLink";
}

QueryLink::QueryLink(const Handle& vardecl,
                   const Handle& body,
                   const Handle& rewrite)
	: QueryLink(HandleSeq{vardecl, body, rewrite})
{}

QueryLink::QueryLink(const Handle& body, const Handle& rewrite)
	: QueryLink(HandleSeq{body, rewrite})
{}

QueryLink::QueryLink(const HandleSeq& hseq, Type t)
	: PatternLink(hseq, t)
{
	init();
}

QueryLink::QueryLink(const Link &l)
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
void QueryLink::extract_variables(const HandleSeq& oset)
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
/* ================================================================= */
/**
 * Execute a QueryLink
 *
 * Given a QueryLink containing variable declarations, a predicate and
 * an implicand, this method will "execute" the implication, matching
 * the predicate, and creating a grounded implicand, assuming the
 * predicate can be satisfied. Thus, for example, given the structure
 *
 *    QueryLink
 *       VariableList
 *          VariableNode "$var0"
 *          VariableNode "$var1"
 *       AndList
 *          etc ...
 *
 * The whole point of the QueryLink is to do nothing more than
 * to indicate the bindings of the variables, and (optionally) limit
 * the types of acceptable groundings for the variables.
 *
 * Use the default implicator to find pattern-matches. Associated truth
 * values are completely ignored during pattern matching; if a set of
 * atoms that could be a ground are found in the atomspace, then they
 * will be reported.
 */
ValueSet QueryLink::do_execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;

	DefaultImplicator impl(as);
	impl.implicand = this->get_implicand();

	/*
	 * The `do_conn_check` flag stands for "do connectivity check"; if the
	 * flag is set, and the pattern is disconnected, then an error will be
	 * thrown. The URE explicitly allows disconnected graphs.
	 *
	 * Set the default to always allow disconnected graphs. This will
	 * get naive users into trouble, but there are legit uses, not just
	 * in the URE, for doing disconnected searches.
	 */
	bool do_conn_check=false;
	if (do_conn_check and 0 == _virtual.size() and 1 < _components.size())
		throw InvalidParamException(TRACE_INFO,
		                            "QueryLink consists of multiple "
		                            "disconnected components!");

	this->PatternLink::satisfy(impl);

	// If we got a non-empty answer, just return it.
	if (0 < impl.get_result_set().size())
	{
		// The result_set contains a list of the grounded expressions.
		// (The order of the list has no significance, so it's really a set.)
		return impl.get_result_set();
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
	const Pattern& pat = this->get_pattern();
	DefaultPatternMatchCB* intu =
		dynamic_cast<DefaultPatternMatchCB*>(&impl);
	if (0 == pat.mandatory.size() and 0 < pat.optionals.size()
	    and not intu->optionals_present())
	{
		ValueSet result;
		result.insert(impl.inst.execute(impl.implicand, true));
		return result;
	}

	return ValueSet();
}

ValuePtr QueryLink::execute(AtomSpace* as, bool silent)
{
	// The result_set contains a list of the grounded expressions.
	// (The order of the list has no significance, so it's really a set.)
	return createLinkValue(do_execute(as, silent));
}

DEFINE_LINK_FACTORY(QueryLink, QUERY_LINK)

/* ===================== END OF FILE ===================== */
