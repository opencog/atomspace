/*
 * PatternLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/FreeLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "BindLink.h"
#include "DualLink.h"
#include "PatternLink.h"
#include "PatternUtils.h"

using namespace opencog;

void PatternLink::common_init(void)
{
	locate_defines(_pat.pmandatory);
	locate_defines(_pat.absents);
	locate_defines(_pat.always);
	locate_defines(_pat.grouping);

	// If there are any defines in the pattern, then all bets are off
	// as to whether it is connected or not, what's virtual, what isn't.
	// The analysis will have to be performed at run-time, so we will
	// skip doing it here.
	if (0 < _pat.defined_terms.size())
	{
		_num_virts = 0;
		_num_comps = 1;
		return;
	}

	_num_virts = _virtual.size();

	// Compute the intersection of literal clauses, and mandatory
	// clauses. This is the set of mandatory clauses that must be
	// present in their literal form.
	for (const PatternTermPtr& ptm : _pat.pmandatory)
	{
		if (ptm->isLiteral() or ptm->isPresent() or ptm->isChoice())
			_fixed.push_back(ptm);
	}

	// Make sure every variable appears in some concrete
	// (non-evaluatable) clause. This consists of non-evaluatable
	// mandatory clauses and clauses which must be absent.
	// Otherwise, we risk not being able to evaluate a clause
	// with some ungrounded variable.
	HandleSeq concrete_clauses;
	for (const PatternTermPtr& ptm : _fixed)
		concrete_clauses.emplace_back(ptm->getQuote());
	for (const PatternTermPtr& ptm : _pat.absents)
		concrete_clauses.emplace_back(ptm->getQuote());
	validate_variables(_variables.varset, concrete_clauses);

	// Split into connected components by splitting virtual clauses.
	get_bridged_components(_variables.varset, _fixed, _pat.absents,
	                       _components, _component_vars);

	// We created the list of fixed clauses for only one reason:
	// to determine pattern connectivity (get_bridged_components)
	// Make this clear by deleting it.
	_fixed.clear();

	// Make sure every variable is in some component.
	check_satisfiability(_variables.varset, _component_vars);

	_num_comps = _components.size();

	// If there is only one connected component, then this can be
	// handled during search by a single PatternLink. The multi-clause
	// grounding mechanism is not required for that case.
	if (1 == _num_comps)
	   make_connectivity_map();

	clauses_get_variables(_pat.pmandatory);
	clauses_get_variables(_pat.absents);
	clauses_get_variables(_pat.always);
	clauses_get_variables(_pat.grouping);

	// Find prunable terms.
	locate_cacheable(_pat.pmandatory);
	locate_cacheable(_pat.absents);
	locate_cacheable(_pat.always);
	locate_cacheable(_pat.grouping);
}


/// The second half of the common initialization sequence
void PatternLink::setup_components(void)
{
	if (_num_comps <= 1) return;

	// If we are here, then set up a PatternLink for each connected
	// component.
	_component_patterns.reserve(_num_comps);
	for (size_t i = 0; i < _num_comps; i++)
	{
		Handle h(createPatternLink(_component_vars[i],
		                           _variables,
		                           _components[i],
		                           _pat.absents));
		_component_patterns.emplace_back(h);
	}
}

/// A body that is an OrLink must be treated as a collection of
/// distinct, unrelated searches. A body that is sequential must
/// run the searches in sequence, and halt when satisfied.
/// Thus, these are always busted up into distinct components.
void PatternLink::disjointed_init(void)
{
	_pat.redex_name = "disjointed PatternLink";

	for (const Handle& h: _body->getOutgoingSet())
	{
		// The variables for that component are just the variables
		// that can be found in that component.
		// XXX FIXME, more correct would be to loop over
		// _pat.clause_variables and add those. Probably makes
		// no difference in most cases.
		FindAtoms fv(_variables.varset);
		fv.search_set(h);
		_component_vars.emplace_back(fv.varset);

		// This one weird little trick will unpack the components
		// that we need. We cannot just push_back `h` into it's own
		// component, because other code assumes the components have
		// already been pattern-compiled, whereas `h` is still raw.
		// Unfortunately, unbundle_clauses sets all sorts of other
		// stuff that we don't need/want, so we have to clobber that
		// every time through the loop.
		// BTW, any `absents`, `always` and `grouping` are probably
		// handled incorrectly, so that's a bug that needs fixing.
		unbundle_clauses(h);

		// Each component consists of the assorted parts.
		// XXX FIXME, this handles `absents`, `always` and `grouping`
		// incorrectly.
		HandleSeq clseq;
		for (const PatternTermPtr& ptm: _pat.pmandatory)
			clseq.push_back(ptm->getHandle());

		_components.emplace_back(clseq);

		// Clear things we don't need.
		// Cannot clear _pat.connected_terms_map;
		// that is used to pin the PatternTerms.
		_pat.pmandatory.clear();
		_pat.clause_variables.clear();
		_pat.connectivity_map.clear();
		_fixed.clear();
		_virtual.clear();
	}

	// We do want to keep a record of the real body.
	_pat.body = _body;

	_num_comps = _components.size();
	setup_components();
}

void PatternLink::init_bottom(void)
{
	// A body that is a ChoiceLink must be treated as a collection of
	// distinct, unrelated searches; the result is a union of the
	// results of the parts. Because most users have trouble
	// distinguishing between menu-choice and logical-or, we will
	// allow OrLink at the top level, as well. Both of these result
	// in the SetUnion of the search results. Of course, this only
	// needs to be worried about, if there is more than one term in
	// the body.  Otherwise, its a no-op.
	Type t = _body->get_type();
	if ((CHOICE_LINK == t or OR_LINK == t) and 1 < _body->get_arity())
	{
		disjointed_init();
		return;
	}

	unbundle_clauses(_body);
	common_init();
	setup_components();
}

void PatternLink::init(void)
{
	// If we are quoted, don't bother to try to do anything.
	if (_quoted) return;

	_pat.redex_name = "anonymous PatternLink";
	ScopeLink::extract_variables(_outgoing);

	// If the _body has not been initialized by ScopeLink, that's
	// because the PatternLink itself was quoted, and thus not
	// actually initializable. This seems ... weird... to me.
	// I'm not convinced its a valid use of Quoting. It seems
	// like a bug. But whatever. System crashes if the body is
	// not set.
	// The root cause is that Nil used PatternLink instead of
	// RuleLink in URE, which made his code run slow and introduced
	// crazy-making into the patterns. We should ditch this, given
	// that teh URE is dead meat anyway.
	if (nullptr == _body) return;

	if (2 < _outgoing.size() or
	   (2 == _outgoing.size() and _outgoing[1] != _body))
	{
		throw InvalidParamException(TRACE_INFO,
		      "Expecting (optional) variable decls and a body; got %s",
		      to_short_string().c_str());
	}

	init_bottom();

#ifdef QDEBUG
	debug_log("PatternLink::init()");
	// logger().fine("Pattern: %s", to_long_string("").c_str());
#endif
}

/* ================================================================= */

void PatternLink::setAtomSpace(AtomSpace* as)
{
	RuleLink::setAtomSpace(as);

	// Can be called with null pointer during destruction.
	if (nullptr == as) return;

	// All patterns will record results into thread-safe queues or to
	// thread-safe deduplicated sets. Use a set by default. User can
	// over-ride later, as desired.
	// Start in closed state, otherwise it will hang in update()
	// when printing.
	UnisetValuePtr svp = createUnisetValue();
	svp->close();

	const Handle& self(get_handle());
	as->set_value(self, self, svp);
}

/* ================================================================= */

/// Special constructor used during just-in-time pattern compilation.
///
/// It assumes that the variables have already been correctly extracted
/// from the body, as appropriate.
PatternLink::PatternLink(const Variables& vars, const Handle& body)
	: RuleLink(HandleSeq(), PATTERN_LINK)
{
	_pat.redex_name = "jit PatternLink";

	_variables = vars;
	_body = body;
	unbundle_clauses(_body);
	common_init();
	setup_components();
}

/* ================================================================= */

/// Special constructor used only to make single concrete pattern
/// components.  We are given the pre-computed components; we only
/// have to store them.
PatternLink::PatternLink(const HandleSet& vars,
                         const Variables& varspec,
                         const HandleSeq& compo,
                         const PatternTermSeq& absts)
	: RuleLink(HandleSeq(), PATTERN_LINK)
{

	// First, lets deal with the vars. We have discarded the original
	// order of the variables, and I think that's OK, because we will
	// never have the substitute aka beta-redex aka putlink method
	// called on us, not directly, at least.  If we need it, then the
	// API will need to be changed...
	// So all we need is the varset, and the subset of the typemap.
	_variables.varset = vars;
	_variables.varseq.clear();
	for (const Handle& v : vars)
	{
		auto it = varspec._typemap.find(v);
		if (it != varspec._typemap.end())
			_variables.unpack_vartype(HandleCast(it->second));
	}

	// Next, the body... there's no `_body` for lambda. The `compo` is
	// the mandatory clauses; we have to reconstruct the optionals.
	for (const Handle& h : compo)
	{
		auto h_is_in = [&](const PatternTermPtr& abs)
		{
			return is_atom_in_tree(abs->getHandle(), h);
		};
		const auto& it = std::find_if(absts.begin(), absts.end(), h_is_in);
		if (it != absts.end())
		{
			// Clone the PatternTerm. We can't use the old one.
			PatternTermPtr term(make_term_tree((*it)->getHandle()));
			if (not term->contained_in(_pat.absents))
			{
				term->markLiteral();
				term->markAbsent();
				_pat.absents.push_back(term);
			}
		}
		else
		{
			PatternTermPtr term(make_term_tree(h));
			if (not term->contained_in(_pat.pmandatory))
				_pat.pmandatory.push_back(term);
		}
	}
	locate_defines(_pat.pmandatory);
	locate_defines(_pat.absents);

	_num_virts = _virtual.size();
	OC_ASSERT (0 == _num_virts, "Must not have any virtuals!");

	_components.emplace_back(compo);
	_num_comps = 1;

	make_connectivity_map();
	_pat.redex_name = "Component of Cartesian product";

	clauses_get_variables(_pat.pmandatory);
	clauses_get_variables(_pat.absents);
}

/* ================================================================= */

/// Constructor that takes a pre-determined set of variables, and
/// a list of clauses to solve.  This is currently kind-of crippled,
/// since no variable type restricions are possible, and no optionals,
/// either.  This is used only for backwards-compatibility API's.
/// XXX No one, except unit tests, use these deprecated API's. These
/// old unit tests should be removed.
PatternLink::PatternLink(const HandleSet& vars,
                         const HandleSeq& clauses)
	: RuleLink(HandleSeq(), PATTERN_LINK)
{
	_variables.varset = vars;
	for (const Handle& clause : clauses)
	{
		PatternTermPtr root_term(make_term_tree(clause));
		_pat.pmandatory.push_back(root_term);
	}
	common_init();
	setup_components();
}

/* ================================================================= */

PatternLink::PatternLink(const Handle& body)
	: RuleLink(HandleSeq({body}), PATTERN_LINK)
{
	init();
}

PatternLink::PatternLink(const Handle& vars, const Handle& body)
	: RuleLink(HandleSeq({vars, body}), PATTERN_LINK)
{
	init();
}

PatternLink::PatternLink(const HandleSeq&& hseq, Type t)
	: RuleLink(std::move(hseq), t)
{
	// Type must be as expected
	if (not nameserver().isA(t, PATTERN_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PatternLink, got %s", tname.c_str());
	}

	// QueryLink, BindLink use a different initialization sequence.
	if (nameserver().isA(t, QUERY_LINK)) return;
	if (DUAL_LINK == t) return;
	init();
}

/* ================================================================= */

void PatternLink::record_mandatory(const PatternTermPtr& term)
{
	// I don't think this check ever triggers;
	// earlier deduplication should have done the trick.
	if (term->contained_in(_pat.pmandatory)) return;
	pin_term(term);
	term->markLiteral();
	_pat.pmandatory.push_back(term);
}

/// Make a note of any clauses that must be present (or absent)
/// in the pattern in their literal form, i.e. uninterpreted.
/// Any evaluatable terms appearing in these clauses are NOT evaluated,
/// but are taken as a request to search for and ground these terms in
/// the form they are given, in their literal form, without evaluation.
bool PatternLink::record_literal(const PatternTermPtr& clause, bool reverse)
{
	const Handle& h = clause->getHandle();
	Type typ = h->get_type();

	// Pull clauses out of a PresentLink
	if ((not reverse and PRESENT_LINK == typ) or
	    (reverse and ABSENT_LINK == typ))
	{
		for (const PatternTermPtr& term : clause->getOutgoingSet())
		{
			const Handle& ph = term->getQuote();
			if (is_constant(_variables.varset, ph)) continue;
			record_mandatory(term);
		}
		return true;
	}

	// Everything under Choice is either a literal, or a grouping of
	// PresentLinks. They are not mandatory, since they exist only in
	// some of the choice branches, but not others. Unless there is
	// only one branch, in which case they become mandatory.
	if (not reverse and CHOICE_LINK == typ)
	{
		// If there is only one choice, then there is effectively no
		// choice at all. Unwrap and discard.
		if (1 == h->get_arity())
		{
			const PatternTermPtr& term(clause->getOutgoingTerm(0));
			const Handle& ph = h->getOutgoingAtom(0);
			Type pht = ph->get_type();
			if (PRESENT_LINK == pht)
			{
				for (const PatternTermPtr& sptm : term->getOutgoingSet())
				{
					const Handle& php = sptm->getHandle();
					if (is_constant(_variables.varset, php)) continue;
					record_mandatory(sptm);
				}
			}
			else if (not is_constant(_variables.varset, ph))
			{
				record_mandatory(term);
			}
			return true;
		}

		// Each of the choices must be findable by the pattern engine.
		for (const PatternTermPtr& term : clause->getOutgoingSet())
		{
			pin_term(term);
			get_clause_variables(term);
		}

		clause->markChoice();
		_pat.pmandatory.push_back(clause);
		return true;
	}

	// Pull clauses out of an AbsentLink
	if ((not reverse and ABSENT_LINK == typ) or
	    (reverse and PRESENT_LINK == typ))
	{
		// We insist on an arity of 1, because anything else is
		// ambiguous: consider absent(A B) is that: "both A and B must
		// be absent"?  Or is it "if any of A and B are absent, then .."
		if (1 != h->get_arity())
			throw InvalidParamException(TRACE_INFO,
				"AbsentLink can have an arity of one only!");

		const Handle& inv(h->getOutgoingAtom(0));
		if (is_constant(_variables.varset, inv)) return true;

		const PatternTermPtr& term(clause->getOutgoingTerm(0));
		pin_term(term);
		term->markLiteral();
		term->markAbsent();
		_pat.absents.push_back(term);
		return true;
	}

	if (reverse and CHOICE_LINK == typ)
		throw InvalidParamException(TRACE_INFO,
			"NotLink-ChoiceLink is not supported yet!");

	// Pull clauses out of an AlwaysLink
	if (not reverse and ALWAYS_LINK == typ)
	    // or (reverse and NEVER_LINK == typ))
	{
		for (PatternTermPtr term: clause->getOutgoingSet())
		{
			const Handle& ah = term->getQuote();
			if (is_constant(_variables.varset, ah)) continue;
			pin_term(term);
			term->markAlways();
			_pat.always.push_back(term);
		}
		return true;
	}

	// Pull clauses out of a GroupLink
	// GroupLinks will have terms that are the groundings that should be
	// grouped together, and also an optional IntervalLink to specify
	// min/max grouping sizes.
	if (not reverse and GROUP_LINK == typ)
	{
		for (PatternTermPtr term: clause->getOutgoingSet())
		{
			const Handle& ah = term->getQuote();

			// In the current code base, there shouldn't be any constants
			// so this check should not be needed.
			if (is_constant(_variables.varset, ah)) continue;
			pin_term(term);
			term->markGrouping();
			_pat.grouping.push_back(term);
		}

		// Look for IntervalLinks. They've been scrubbed from the
		// Pattern term because they're constants; we have to look
		// at the GrouplLink itself to find them.
		long glo = LONG_MAX;
		long ghi = 0;
		for (const Handle& ah: h->getOutgoingSet())
		{
			if (INTERVAL_LINK != ah->get_type())
				continue;
			NumberNodePtr nlo(NumberNodeCast(ah->getOutgoingAtom(0)));
			NumberNodePtr nhi(NumberNodeCast(ah->getOutgoingAtom(1)));
			long ilo = (long) std::round(nlo->get_value());
			long ihi = (long) std::round(nhi->get_value());
			if (glo > ilo) glo = ilo;
			if (ghi < ihi) ghi = ihi;
			if (ihi < 0) ghi = LONG_MAX;
		}
		if (0 != ghi)
		{
			_pat.group_min_size = glo;
			_pat.group_max_size = ghi;
		}
		return true;
	}

	// Handle in-line variable declarations.
	if (not reverse and TYPED_VARIABLE_LINK == typ)
	{
		// We have to erase first, else it gets duplicated.
		_variables.erase(h->getOutgoingAtom(0));
		_variables.validate_vardecl(h);
		return true;
	}

	return false;
}

/// Search for any PRESENT_LINK, ABSENT_LINK and CHOICE_LINK's that are
/// recursively embedded inside some evaluatable clause.  Note these as
/// literal, groundable clauses. `record_literal` does this.
///
/// If there weren't any literal Present, Absent or Choice Links, (i.e.
/// if `record_literal` didn't spot anything) then some later stages
/// will attempt to fish out any literal-like terms hidden inside of
/// of evaluatable clauses.
///
/// What we do here is:
/// * If a clause is not evaluatable, then assume `Present` was intended.
/// * If it is anything else, assume some later stage will evaluate it.
///
bool PatternLink::unbundle_clauses_rec(const PatternTermPtr& term,
                                       const TypeSet& connectives,
                                       bool reverse)
{
	const Handle& bdy = term->getHandle();
	Type t = bdy->get_type();

	if (connectives.find(t) == connectives.end())
		return false;

	if (NOT_LINK == t) reverse = not reverse;

	bool recorded = true;
	for (const PatternTermPtr& pto : term->getOutgoingSet())
	{
		if (record_literal(pto, reverse)) continue;
		if (unbundle_clauses_rec(pto, connectives, reverse)) continue;

		recorded = false;
	}
	return recorded;
}

/// Unpack the clauses.
///
/// The predicate is either an AndLink of clauses to be satisfied, or a
/// single clause. Other link types, such as OrLink and SequentialAnd,
/// are treated as single clauses; unpacking them here would lead to
/// confusion in the pattern matcher.  This is because the AndLink is
/// an unordered set, and clauses can be grounded in an arbitrary order;
/// whereas SequentialAnd's must be grounded and evaluated sequentially.
///
/// The overall process makes built-in assumptions about using the
/// TermMatchMixin, which gives the boolean operators their classical
/// logic interpretation. In principle, other interpretations are
/// possible (e.g. linear logic, or any number of the modal logics),
/// but these are not currently supported in this code base. Supporting
/// probably requires a "LinearLogicPatternLink" which will borrow much
/// of the code below, but not all, and work with a LinearTermMixin
/// callback class to complete the matching process.
void PatternLink::unbundle_clauses(const Handle& hbody)
{
	_pat.body = hbody;

	// A collection of clauses, all of which must be satisfied.
	if (AND_LINK == hbody->get_type())
	{
		TypeSet connectives({AND_LINK, OR_LINK, NOT_LINK});

		const HandleSeq& oset = hbody->getOutgoingSet();

		// De-duplicate repeated clauses in the search pattern.
		HandleSet dedupe;
		for (const Handle& ho : oset)
			dedupe.insert(ho);

		for (const Handle& ho : dedupe)
		{
			PatternTermPtr clause(make_term_tree(ho));
			if (not clause->contained_in(_pat.pmandatory) and
			    not is_constant(_variables.varset, ho) and
			    not record_literal(clause) and
			    not unbundle_clauses_rec(clause, connectives))
			{
				_pat.pmandatory.push_back(clause);

				if (not clause->isVirtual())
					_fixed.emplace_back(clause);
			}
		}
		return;
	}

	// Fish out the PresentLink's, and add them to the
	// list of clauses to be grounded.
	PatternTermPtr clause(make_term_tree(hbody));
	if (record_literal(clause))
		return;

	TypeSet connectives({AND_LINK, SEQUENTIAL_AND_LINK,
	                     OR_LINK, SEQUENTIAL_OR_LINK, NOT_LINK});

	// BUG - XXX FIXME. This extracts PresentLink's from the
	// Sequentials. This is not really correct, because the
	// evaluation of the sequential might terminate *before*
	// the PresentLink is reached. Whereas the current design
	// of the clause-walking will run the PresentLink before
	// running the sequential. So that's a bug.
	if (not unbundle_clauses_rec(clause, connectives) and
	    not is_constant(_variables.varset, hbody))
	{
		_pat.pmandatory.push_back(clause);

		if (not clause->isVirtual())
			_fixed.emplace_back(clause);
	}
}

/* ================================================================= */

void PatternLink::locate_defines(const PatternTermSeq& clauses)
{
	for (const PatternTermPtr& ptm: clauses)
	{
		const Handle& clause = ptm->getQuote();
		FindAtoms fdpn(DEFINED_PREDICATE_NODE, DEFINED_SCHEMA_NODE, true);
		fdpn.stopset.insert(SCOPE_LINK);
		fdpn.search_set(clause);

		for (const Handle& sh : fdpn.varset)
			_pat.defined_terms.insert(sh);
	}
}

/* ================================================================= */
/**
 * Locate cacheable clauses. These are clauses whose groundings can be
 * cached for later re-use. To qualify for being cacheable, the clause
 * must not contain any evaluatable terms (as that would result in a
 * changing grounding), cannot contain any unordered or choice links
 * (as these have multiple groundings) and the clause can only contain
 * one variable (there is no use-cases for two or more variables, at
 * this time. Similarly, no Globs either.)
 *
 * Caching is used to "prune" or "cut" clauses during search; once the
 * joining variable is known, the "up" exploration can be skipped, and
 * pruned clause re-attached with the known grounding.
 *
 * This is kind-of the opposite of `is_virtual()`.
 */
void PatternLink::locate_cacheable(const PatternTermSeq& clauses)
{
	for (const PatternTermPtr& ptm: clauses)
	{
		if (not ptm->isLiteral() and not ptm->isPresent() and
		    not ptm->isChoice() and not ptm->isAbsent()) continue;

		const Handle& claw = ptm->getHandle();

		// Caching works fine, if there are UnorderedLinks. However,
		// if there is a lot of them, so that the engine is exploring
		// a combinatorially deep arrangement, then caching becomes
		// counter-productive.  Based on running UnorderedUTest, the
		// knee in the curve is at 4 or fewer UnorderedLinks in a clause.
		// Note that UnorderedUTest has some very unusual patterns,
		// exploring many tens of thousands of combinations, something
		// that most ussers will surely almost never do :-)
		if (4 < contains_atomtype_count(claw, UNORDERED_LINK)) continue;

		_pat.cacheable_clauses.insert(claw);
	}
}

/* ================================================================= */

/// get_clause_variables -- Make note of the variables in this term.
/// This is used at runtime, to determine if the clause has been fully
/// grounded (or not).
void PatternLink::get_clause_variables(const PatternTermPtr& ptm)
{
	const Handle& hcl = ptm->getQuote();
	HandleSet vset = get_free_variables(hcl);

	// Put them into a sequence; any fixed sequence will do.
	HandleSeq vseq;
	for (const Handle& v: vset)
	{
		if (_variables.varset.end() != _variables.varset.find(v))
			vseq.emplace_back(v);
	}

	_pat.clause_variables.insert({ptm, vseq});
}

/// get_clause_variables -- for every clause, record the variables in it.
void PatternLink::clauses_get_variables(const PatternTermSeq& clauses)
{
	for (const PatternTermPtr& ptm : clauses)
		get_clause_variables(ptm);
}

/* ================================================================= */
/**
 * Make sure that each declared variable appears in some clause.
 * We can't ground variables that don't show up in a clause; there's
 * just no way to know.  Throw, because they are presumably there due
 * to programmer error. Quoted variables are constants, and so don't
 * count.
 */
void PatternLink::validate_variables(HandleSet& vars,
                                     const HandleSeq& clauses)
{
	for (const Handle& v : vars)
	{
		if (not is_unquoted_in_any_tree(clauses, v))
		{
			Handle tmp(v);
			vars.erase(v);
			throw InvalidParamException(TRACE_INFO,
			   "The variable %s does not appear (unquoted) in any clause!",
			   tmp->to_short_string().c_str());
		}
	}
}

/* ================================================================= */

/// is_virtual -- check to see if a clause is virtual.
///
/// A clause is virtual if it has two or more unquoted, unscoped
/// variables in it. Otherwise, it can be evaluated on the spot.
///
/// At this time, the pattern matcher does not support mathematical
/// optimization within virtual clauses.
/// See https://en.wikipedia.org/wiki/Mathematical_optimization
///
/// So, virtual clauses are already one step towards full support
/// for optimization, as they do enable certain kinds of brute-force
/// search across disconnected components. So, there is partial
/// support, for simple kinds of optimization problems. It would
/// take more work and refactoring to support more.  Thus, for now,
/// just throw an error when the more complex optimzation problems
/// are encountered.
///
/// To add support, we would have to split executable clauses into
/// component graphs, the same way we currently split VirtualLinks.
///
bool PatternLink::is_virtual(const Handle& clause)
{
	size_t nfree = num_unquoted_unscoped_in_tree(clause, _variables.varset);
	if (2 > nfree) return false;

	// IdenticalLinks can bridge over their two sides.
	// So we treat them as an unusual but not really virtual link.
	if (IDENTICAL_LINK == clause->get_type()) return false;

	size_t nsub = 0;
	size_t nsolv = 0;
	size_t nvar = 0;
	for (const Handle& sub: clause->getOutgoingSet())
	{
		size_t nv = num_unquoted_unscoped_in_tree(sub, _variables.varset);
		if (0 < nv)
		{
			nsub++;
			if (sub->is_executable()) nsolv++;
			if (VARIABLE_NODE == sub->get_type()) nvar++;
		}
	}
	if (2 <= nsolv or (1 == nsolv and 0 < nvar))
	{
		throw InvalidParamException(TRACE_INFO,
			"This optimization problem currently not supported!");
	}

	return true;
}

/* ================================================================= */

/// Return true if the term has variables in it that do not yet appear
/// in any non-evaluatable mandatory clause. If there are such
/// variables, we either have to find non-evaluatable terms that hold
/// them, or add those variables directly, as mandatory terms.
bool PatternLink::need_dummies(const PatternTermPtr& ptm)
{
	// Are there any variables below us?
	// Do they already appear in some existing mandatory term?
	// If not, then we have to go fishing for fixed terms,
	// or add dummies.
	HandleSet vset = get_free_variables(ptm->getQuote());
	for (const Handle& v: vset)
	{
		bool found_this_v = false;
		for (const PatternTermPtr& man : _pat.pmandatory)
		{
			HandleSet vman = get_free_variables(man->getQuote());
			if (vman.end() != vman.find(v))
			{
				found_this_v = true;
				break;
			}
		}
		if (not found_this_v)
			return true;
	}
	return false;
}

/// Add implicit fixed terms. The pattern might consist only of
/// evaluatable clauses. Unless we find some constant Atom inside of
/// it, we risk having to perform a very large search trying to find
/// something that makes the evaluatable clauses true.  Thus, it is
/// highly advantageous to find some constant term on which to start
/// the search. That's what we try to do here.
///
/// This recursively explores the term, attempting to find "ordinary"
/// links that have exactly one variable in them, and at least one
/// other atom. Since this other atom appears inside a non-evaluatable
/// "ordinary", it must necessarily appear in the AtomSpace. Thus, this
/// kind of "ordinary" link counts as a fixed term. (A link is
/// "ordinary" if it's not evaluatable and not a function.)
///
bool PatternLink::add_unaries(const PatternTermPtr& ptm)
{
	const Handle& h = ptm->getHandle();

	// Ignore literals inside of Choice and Always; these should have
	// been handled earlier. Ditto for Present, Absent.
	// The Sequentials are weird from a search perspective: they appear
	// (should only appear) in SatisfactionLinks, and have their own
	// distinct logic to them. So if we're inside of one of these,
	// we drop out. Probably should refactor the code so that we are
	// not even called for any of these cases.
	Type t = h->get_type();
	if (CHOICE_LINK == t or ALWAYS_LINK == t) return false;
	if (GROUP_LINK == t) return false;
	if (ABSENT_LINK == t or PRESENT_LINK == t) return false;
	if (SEQUENTIAL_AND_LINK == t or SEQUENTIAL_OR_LINK == t) return false;

	PatternTermPtr parnt = ptm->getParent();
	while (parnt)
	{
		const Handle& ph = parnt->getHandle();
		if (nullptr == ph) break;
		Type pt = ph->get_type();
		if (SEQUENTIAL_AND_LINK == pt or SEQUENTIAL_OR_LINK == pt) return false;
		parnt = parnt->getParent();
	}

	if (EVALUATION_LINK == t)
	{
		OC_ASSERT(0 < h->get_arity(),
			"Don't now what to do with empty EvlauationLinks");

		if (PREDICATE_NODE == h->getOutgoingAtom(0)->get_type())
		{
			// deduplicate on the fly.
			if (not ptm->contained_in(_pat.pmandatory))
				_pat.pmandatory.push_back(ptm);

			// XXX Shouldn't we be adding this to _fixed, too?
			return true;
		}

		// EvaluationLinks with evaluatable predicates cannot
		// be found in the AtomSpace. But perhaps their arguments
		// might be. So fall through and look at those.
	}

	// Try to add any kind of "ordinary" link that contains exactly
	// one variable in it, and at least one non-variable term in it.
	// (thus, an arity of 2 or more.)  It's "ordinary" if it is not
	// evaluatable or executable.
	if (not nameserver().isA(t, NODE) and
	    1 < h->get_arity() and
	    not nameserver().isA(t, EVALUATABLE_LINK) and
	    not nameserver().isA(t, FREE_LINK) and
	    1 == num_unquoted_unscoped_in_tree(h, _variables.varset))
	{
		// deduplicate on the fly.
		if (not ptm->contained_in(_pat.pmandatory))
			_pat.pmandatory.push_back(ptm);

		// XXX Shouldn't we be adding this to _fixed, too?
		return true;
	}

	bool added = false;
	for (const PatternTermPtr& sub: ptm->getOutgoingSet())
	{
		added = added or add_unaries(sub);
	}
	return added;
}

/// Add dummy clauses for patterns that would otherwise not have any
/// non-evaluatable clauses.  One example of such is
///
///    (GetLink (GreaterThan (Number 42) (Variable $x)))
///
/// The only clause here is the GreaterThan, and it is virtual
/// (evaluatable) so we know that in general it cannot be found in
/// the atomspace.   Due to the pattern-matcher design, matching will
/// fail unless there is at least one PresentLink/AbsentLink clause.
/// We can infer, from the above, that the Variable $x must be in the
/// atomspace, so we just add it here, as a dummy clause that can be
/// trivially satisfied.  This can be done generically, without changing
/// search results, for any variable that is not in an AbsentLink.
/// (Always adding can harm performance, so we don't do it unless we
/// absolutely have to.)
///
/// Another example is
///
///    (GetLink (Identical (Variable "$whole") (Implication ...)))
///
/// where the ImplicationLink may itself contain more variables.
/// If the ImplicationLink is suitably simple, it can be added
/// as an ordinary clause, and searched for as if it was "present".
///
/// XXX FIXME: the code here assumes that the situation is indeed
/// simple: more complex cases are not handled correctly.  Doing this
/// correctly would require iterating again, and examining the
/// contents of the left and right side of the IdenticalLink... ugh.
///
/// XXX The situation here is also very dangerous: without any
/// type constraints, we risk searching atoms created in the scratch
/// atomspace, resulting in infinite recursion and a blown stack.
/// Not clear how to avoid that...
///
void PatternLink::add_dummies(const PatternTermPtr& ptm)
{
	const Handle& h = ptm->getHandle();
	Type t = h->get_type();

	if (not nameserver().isA(t, VIRTUAL_LINK)
	    or SATISFACTION_LINK == t)
		return;

	for (const PatternTermPtr& sub: ptm->getOutgoingSet())
	{
		const Handle& sh = sub->getHandle();
		if (can_evaluate(sh)) continue;
		if (not any_unquoted_unscoped_in_tree(sh, _variables.varset))
			continue;

		_fixed.emplace_back(sub);
	}
}

/* ================================================================= */
/**
 * Create a map that holds all of the clauses that a given atom
 * participates in.  In other words, it indicates all the places
 * where an atom is shared by multiple trees, and thus establishes
 * how the trees are connected.
 *
 * This is used for only one purpose: to find the next unsolved
 * clause. Perhaps this could be simplified somehow ...
 */
void PatternLink::make_connectivity_map(void)
{
	for (const PatternTermPtr& ptm : _pat.pmandatory)
		make_map_recursive(ptm->getHandle(), ptm);
	for (const PatternTermPtr& ptm : _pat.absents)
		make_map_recursive(ptm->getHandle(), ptm);

	// Save some minor amount of space by erasing those atoms that
	// participate in only one clause. These atoms cannot be used
	// to determine connectivity between clauses, and so are un-needed.
	auto it = _pat.connectivity_map.begin();
	auto end = _pat.connectivity_map.end();
	while (it != end)
	{
		if (1 == _pat.connectivity_map.count(it->first))
			it = _pat.connectivity_map.erase(it);
		else
			it++;
	}
}

void PatternLink::make_map_recursive(const Handle& var,
                                     const PatternTermPtr& root)
{
	_pat.connectivity_map.emplace(var, root);

	if (var->is_link())
	{
		for (const Handle& var: var->getOutgoingSet())
			make_map_recursive(var, root);
	}
}

/// Make sure that every variable appears in some groundable clause.
/// Variables have to be grounded before an evaluatable clause
/// containing them can be evaluated. Throw an error if some variable
/// wasn't explicitly specified in a groundable clause.
void PatternLink::check_satisfiability(const HandleSet& vars,
                                       const HandleSetSeq& compvars)
{
	// Compute the set-union of all component vars.
	HandleSet vunion;
	for (const HandleSet& vset : compvars)
		vunion.insert(vset.begin(), vset.end());

	// Is every variable in some component? The user can give
	// us mal-formed things like this:
	//    (Bind (Evaluation (GroundedPredicate "scm: foo")
	//               (List (Variable "X") (Variable "Y")))
	//          (Concept "OK"))
	// and we could add `(Variable "X") (Variable "Y")` with the
	// `add_dummies()` method above. But if we did, it would be
	// an infinite loop that blows out the stack, because X and Y
	// are not constrained, and match everything, including the
	// temporary terms created during search... so we do NOT
	// `add_dummies()` this case. See issue #1420 for more.
	for (const Handle& v : vars)
	{
		const auto& it = vunion.find(v);
		if (vunion.end() == it)
			throw SyntaxException(TRACE_INFO,
				"Poorly-formed query; a variable declaration for %s is needed!",
				v->to_short_string().c_str());
	}
}

PatternTermPtr PatternLink::make_term_tree(const Handle& term)
{
	PatternTermPtr top_term(createPatternTerm());
	PatternTermPtr root_term(top_term->addOutgoingTerm(term));
	make_term_tree_recursive(root_term, root_term);
	return root_term;
}

// Temporary helper function (under construction)
static inline bool can_eval_or_present(const Handle& h)
{
	return can_evaluate(h) or PRESENT_LINK == h->get_type();
}

void PatternLink::pin_term(const PatternTermPtr& ptm)
{
	pin_term_recursive(ptm, ptm);
}

void PatternLink::pin_term_recursive(const PatternTermPtr& ptm,
                                     const PatternTermPtr& root)
{
	Handle h(ptm->getHandle());
	_pat.connected_terms_map[{h, root}].emplace_back(ptm);
	for (const PatternTermPtr& stm : ptm->getOutgoingSet())
		pin_term_recursive(stm, root);
}

void PatternLink::make_term_tree_recursive(const PatternTermPtr& root,
                                           PatternTermPtr& ptm)
{
	// `h` is usually the same as `term`, unless there's quotation.
	const Handle& h(ptm->getHandle());
	_pat.connected_terms_map[{h, root}].emplace_back(ptm);

	// If the current node is a bound variable, store this as a
	// bool flag, recursively, up into the parent tree, for later use.
	// The addBoundVariable() method walks upwards into the parent to
	// set this flag.
	Type t = h->get_type();
	if ((VARIABLE_NODE == t or GLOB_NODE == t)
	    and _variables.varset.end() != _variables.varset.find(h)
	    and not ptm->isQuoted())
	{
		ptm->addBoundVariable();

		// It's globby, if it is explicitly a GLOB_NODE, or if
		// it has a non-trivial matching interval.
		if (GLOB_NODE == t or _variables.is_globby(h))
			ptm->addGlobbyVar();
		return;
	}

	// Signatures are just variables without a name. They are simpler
	// to deal with, overall.
	if (SIGN_NODE == t or SIGNATURE_LINK == t)
	{
		ptm->addAnonVar();
		return;
	}

	// If the term is unordered, all parents must know about it.
	if (nameserver().isA(t, UNORDERED_LINK))
	{
		// If there's a GlobNode in here, make sure there's only one.
		// Unordered links with globs hit the "sparse" pattern match.
		if (ptm->hasGlobbyVar())
		{
			size_t cnt = 0;
			const Handle& hu = ptm->getHandle();
			for (const Handle& ho : hu->getOutgoingSet())
			{
				if (nameserver().isA(ho->get_type(), GLOB_NODE))
					cnt++;
			}
			if (1 < cnt)
				throw InvalidParamException(TRACE_INFO,
					"Unordered patterns can have at most ONE GlobNode\n"
					"(because having more does not make sense). Got: %s",
					hu->to_short_string().c_str());
		}
		ptm->addUnorderedLink();
	}

	// If the parent isn't evaluatable, it makes no sense to
	// mark the child evaluatable. The problem here is that
	// users insert stray AndLinks into random places.
	const PatternTermPtr& parent = ptm->getParent();
	if ((parent->getHandle() == nullptr or parent->hasEvaluatable())
	    and not ptm->isQuoted() and can_evaluate(h))
	{
		// If its an AndLink, make sure that all of the children are
		// evaluatable. The problem is .. users insert AndLinks into
		// random places...
		bool is_ev = true;
		if (AND_LINK == t)
		{
			for (const Handle& ho : h->getOutgoingSet())
				if (not can_eval_or_present(ho))
				{
					is_ev = false;
					break;
				}
		}

		if (is_ev)
		{
			_pat.have_evaluatables = true;
			ptm->addEvaluatable();

			// XXX FIXME -- this is wrong. What we really want is to
			// identify those clauses that bridge across multiple
			// components... not everything here does so. The
			// get_bridged_components() should be modified to
			// identify the bridging clauses... XXX Wait, maybe this
			// does not need to be fixed, since the component splitter
			// will not split these. So we're good, I think ...
			if (parent->getHandle() == nullptr or not parent->isVirtual())
			{
				if (is_virtual(h))
				{
					_virtual.emplace_back(h);
					ptm->markVirtual();
				}
				else if (IDENTICAL_LINK == t)
					ptm->markIdentical();
			}
		}
	}

	// Recurse down to the tips. ... after the evaluatable markup above.
	if (h->is_link())
	{
		// Remove constants from PresentLink, as they are pointless.
		// The URE frequently puts them there.
		// The gotcha is that some things look const, but that is only
		// because they involve variables that are bound to some other
		// scope up above. Those are NOT actually const. This is not
		// particularly well-thought out. Might be buggy...
		bool chk_const = (PRESENT_LINK == t or ABSENT_LINK == t);
		chk_const = chk_const or ALWAYS_LINK == t or GROUP_LINK == t;
		chk_const = chk_const and not parent->hasAnyEvaluatable();
		chk_const = chk_const and not ptm->isQuoted();

		for (const Handle& ho: h->getOutgoingSet())
		{
			if (chk_const and is_constant(_variables.varset, ho)) continue;
			PatternTermPtr po(ptm->addOutgoingTerm(ho));
			make_term_tree_recursive(root, po);
		}
	}

	if (PRESENT_LINK == t)
	{
		ptm->markPresent();
		return;
	}

	// If a term is literal then the corresponding pattern term
	// should be also.
	if (CHOICE_LINK == t)
	{
		for (PatternTermPtr& optm: ptm->getOutgoingSet())
		{
			if (PRESENT_LINK == optm->getHandle()->get_type())
				optm->markPresent();
		}
		ptm->markChoice();
		return;
	}

	// Much like the ChoiceLink above, except that the term itself
	// cannot be marked as a choice.
	if (OR_LINK == t)
	{
		for (PatternTermPtr& optm: ptm->getOutgoingSet())
		{
			if (PRESENT_LINK == optm->getHandle()->get_type())
				optm->markPresent();
		}
		return;
	}

	// Skip the second pass below, when exploring the insides of an
	// OrLink.  The problem is that add_unaries below will add terms
	// inside the OrLink as mandatory, when of course, they are not.
	const Handle& hpnt(parent->getHandle());
	if (hpnt and OR_LINK == hpnt->get_type()) return;

	// Second pass for evaluatables - this time to mark the left-overs
	// as literals. We need to do this AFTER recursion, not before.
	if ((parent->getHandle() == nullptr or parent->hasEvaluatable())
	    and not ptm->isQuoted() and can_evaluate(h))
	{
		if (not need_dummies(ptm)) return;

		// If its an AndLink, make sure that all of the children are
		// evaluatable. The problem is .. users insert AndLinks into
		// random places...
		if (AND_LINK == t)
		{
			for (const PatternTermPtr& ptc : ptm->getOutgoingSet())
				if (ptc->isQuoted() or not can_eval_or_present(ptc->getHandle()))
				{
					ptm->markLiteral();
					return;
				}
		}

		// If the evaluatables have literal-ish subterms, add those.
		if (add_unaries(ptm)) return;

		// If the above failed, then try adding dummy variables.
		add_dummies(ptm);
		return;
	}

	ptm->markLiteral();
}

/* ================================================================= */
/**
 * Check that all clauses are connected
 */
void PatternLink::check_connectivity(const HandleSeqSeq& components)
{
	if (1 == components.size()) return;

	// Users are going to be stumped by this one, so print
	// out a verbose, user-freindly debug message to help
	// them out.
	std::stringstream ss;
	ss << "Pattern is not connected! Found "
	   << components.size() << " components:";
	int cnt = 1;
	for (const auto& comp : components)
	{
		ss << std::endl << "Connected component " << cnt
		   << " consists of ----------------:";
		for (Handle h : comp) ss << std::endl << h->to_string();
		cnt++;
	}
	throw InvalidParamException(TRACE_INFO, "%s", ss.str().c_str());
}

/* ================================================================= */

void PatternLink::debug_log(std::string msg) const
{
	if (not logger().is_fine_enabled())
		return;

	// Log the pattern ...
	logger().fine("Pattern debug log from '%s'", msg.c_str());
	logger().fine("Pattern '%s' summary:",
	              _pat.redex_name.c_str());
	logger().fine("%lu mandatory terms", _pat.pmandatory.size());
	logger().fine("%lu absent clauses", _pat.absents.size());
	logger().fine("%lu always clauses", _pat.always.size());
	logger().fine("%lu grouping clauses", _pat.grouping.size());
	logger().fine("%lu fixed clauses", _fixed.size());
	logger().fine("%lu virtual clauses", _num_virts);
	logger().fine("%lu components", _num_comps);
	logger().fine("%lu variables\n", _variables.varset.size());

	int num = 0;
	std::string str = "\n";
	for (const PatternTermPtr& ptm : _pat.pmandatory)
	{
		str += "================ Mandatory clause " + std::to_string(num) + ":";
		if (ptm->hasAnyEvaluatable()) str += " (evaluatable)";
		str += "\n";
		str += ptm->to_full_string() + "\n\n";
		num++;
	}

	for (const PatternTermPtr& ptm : _pat.absents)
	{
		str += "================ Absent clause " + std::to_string(num) + ":\n";
		str += ptm->to_full_string() + "\n\n";
		num++;
	}

	for (const PatternTermPtr& ptm : _pat.always)
	{
		str += "================ Always clause " + std::to_string(num) + ":";
		if (ptm->hasAnyEvaluatable()) str += " (evaluatable)";
		str += "\n";
		str += ptm->to_full_string() + "\n\n";
		num++;
	}

	for (const PatternTermPtr& ptm : _pat.grouping)
	{
		str += "================ Grouping clause " + std::to_string(num) + ":";
		if (ptm->hasAnyEvaluatable()) str += " (evaluatable)";
		str += "\n";
		str += ptm->to_full_string() + "\n\n";
		num++;
	}

	str.pop_back();
	logger().fine() << str;

	// Print out the bound variables in the predicate.
	for (const Handle& h : _variables.varset)
	{
		if (h->is_node())
			logger().fine() << "Bound var: " << h->to_short_string();
	}

	if (_variables.varset.empty())
		logger().fine("There are no bound vars in this pattern");
	else
		logger().fine() << "Type declarations are:\n"
		                << oc_to_string(_variables);
}

DEFINE_LINK_FACTORY(PatternLink, PATTERN_LINK)

// XXX FIXME: debug_log() above is more readable than the below.
std::string PatternLink::to_long_string(const std::string& indent) const
{
	std::string indent_p = indent + oc_to_string_indent;
	std::stringstream ss;
	ss << to_string(indent) << std::endl;
	ss << indent << "_pat:" << std::endl
	   << oc_to_string(_pat, indent_p) << std::endl;
	// ss << indent << "_fixed:" << std::endl
	//   << oc_to_string(_fixed, indent_p) << std::endl;
	ss << indent << "_num_virts = " << _num_virts << std::endl;
	ss << indent << "_virtual:" << std::endl
	   << oc_to_string(_virtual, indent_p) << std::endl;
	ss << indent << "_num_comps = " << _num_comps << std::endl;
	ss << indent << "_components:" << std::endl
	   << oc_to_string(_components, indent_p) << std::endl;
	ss << indent << "_component_vars:" << std::endl
	   << oc_to_string(_component_vars, indent_p) << std::endl;
	ss << indent << "_component_patterns:" << std::endl
	   << oc_to_string(_component_patterns, indent_p);
	return ss.str();
}

std::string oc_to_string(const PatternLink& pl, const std::string& indent)
{
	return pl.to_long_string(indent);
}

/* ===================== END OF FILE ===================== */
