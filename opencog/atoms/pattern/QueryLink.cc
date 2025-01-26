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

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/query/Implicator.h>
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

	// If we are quoted, don't bother to try to do anything.
	if (_quoted) return;

	size_t sz = _outgoing.size();
	if (sz < 2)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at least two, got %d", sz);

	_pat.redex_name = "anonymous QueryLink";
	RuleLink::extract_variables(_outgoing);

	init_bottom();

#ifdef QDEBUG
	logger().fine("Query: %s", to_long_string("").c_str());
#endif
}

QueryLink::QueryLink(const Handle& vardecl,
                   const Handle& body,
                   const Handle& rewrite)
	: QueryLink(HandleSeq{vardecl, body, rewrite})
{}

QueryLink::QueryLink(const Handle& body, const Handle& rewrite)
	: QueryLink(HandleSeq{body, rewrite})
{}

QueryLink::QueryLink(const HandleSeq&& hseq, Type t)
	: PatternLink(std::move(hseq), t)
{
	init();
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
ContainerValuePtr QueryLink::do_execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;

	/*
	 * The `do_conn_check` flag stands for "do connectivity check"; if the
	 * flag is set, and the pattern is disconnected, then an error will be
	 * thrown. The URE explicitly allows disconnected graphs.
	 *
	 * Set the default to always allow disconnected graphs. This will
	 * get naive users into trouble, but there are legit uses, not just
	 * in the URE, for doing disconnected searches.
	 */
	bool do_conn_check = false;
	if (do_conn_check and 0 == _virtual.size() and 1 < _components.size())
		throw InvalidParamException(TRACE_INFO,
		                            "QueryLink consists of multiple "
		                            "disconnected components!");

	// Where shall we place results? Why, right here!
	ValuePtr vp(getValue(get_handle()));
	ContainerValuePtr cvp(ContainerValueCast(vp));
	if (nullptr == cvp)
		throw RuntimeException(TRACE_INFO,
			"Expecting QueueValue for results!");

	Implicator impl(as, cvp);
	impl.implicand = this->get_implicand();

	try
	{
		impl.satisfy(PatternLinkCast(get_handle()));
	}
	catch(const StandardException& ex)
	{
		std::string msg =
			"Exception during execution of pattern\n";
		msg += to_string();
		msg += "\nException was:\n";
		msg += ex.get_message();
		ex.set_message(msg.c_str());
		throw;
	}

	// If we got a non-empty answer, just return it.
	OC_ASSERT(cvp->is_closed(), "Unexpected queue state!");
	if (0 < cvp->size())
		return cvp;

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
	TermMatchMixin* intu =
		dynamic_cast<TermMatchMixin*>(&impl);
	if (0 == pat.pmandatory.size() and 0 < pat.absents.size()
	    and not intu->optionals_present())
	{
		cvp->open();
		for (const Handle& himp: impl.implicand)
			cvp->add(std::move(impl.inst.execute(himp, true)));
		cvp->close();
		return cvp;
	}

	return cvp;
}

ValuePtr QueryLink::execute(AtomSpace* as, bool silent)
{
	return do_execute(as, silent);
}

DEFINE_LINK_FACTORY(QueryLink, QUERY_LINK)

/* ===================== END OF FILE ===================== */
