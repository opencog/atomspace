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

#include <opencog/util/Logger.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FreeLink.h>
#include <opencog/atomutils/FindUtils.h>

#include "PatternLink.h"
#include "PatternUtils.h"

using namespace opencog;

void PatternLink::common_init(void)
{
	locate_defines(_pat.clauses);
	locate_globs(_pat.clauses);

	// If there are any defines in the pattern, then all bets are off
	// as to whether it is connected or not, what's virtual, what isn't.
	// The analysis will have to be performed at run-time, so we can
	// skip doing it here.
	if (0 < _pat.defined_terms.size())
	{
		_num_comps = 1;
		return;
	}

	validate_clauses(_varlist.varset, _pat.clauses, _pat.constants);
	extract_optionals(_varlist.varset, _pat.clauses);

	// Locate the black-box clauses.
	unbundle_virtual(_varlist.varset, _pat.cnf_clauses,
	                 _fixed, _virtual, _pat.black);
	_num_virts = _virtual.size();

	// unbundle_virtual does not handle connectives. Here, we assume that
	// we are being run with the DefaultPatternMatchCB, and so we assume
	// that the logical connectives are AndLink, OrLink and NotLink.
	// Tweak the evaluatable_holders to reflect this.
	std::set<Type> connectives({AND_LINK, SEQUENTIAL_AND_LINK,
	                            OR_LINK, SEQUENTIAL_OR_LINK, NOT_LINK});
	trace_connectives(connectives, _pat.clauses);

	// Split the non-virtual clauses into connected components
	get_connected_components(_varlist.varset, _fixed,
	                         _components, _component_vars);
	_num_comps = _components.size();

	// Make sure every variable is in some component.
	check_satisfiability(_varlist.varset, _component_vars);

	// If there is only one connected component, then this can be
	// handled during search by a single PatternLink. The multi-clause
	// grounding mechanism is not required for that case.
	if (1 == _num_comps)
	{
		// Each component is in connection-order. By re-assigning to
		// _pat.cnf_clauses, they get placed in that order, this giving
		// a minor performance boost during clause traversal.
		// Gurk. This does not work currently; the evaluatables have been
		// stripped out of the component. I think this is a bug ...
		// Is this related to the other XXX for validate_clauses??
		// _pat.cnf_clauses = _components[0];
	   make_connectivity_map(_pat.cnf_clauses);
	}

	make_term_trees();
}


/// The second half of the common initialization sequence
void PatternLink::setup_components(void)
{
	if (1 == _num_comps) return;

	// If we are here, then set up a PatternLink for each connected
	// component.
	//
	// There is a pathological case where there are no virtuals, but
	// there are multiple disconnected components.  I think that this is
	// a user-error, but in fact PLN does have a rule which wants to
	// explore that combinatoric explosion, on purpose. So we have to
	// allow the multiple disconnected components for that case.
	_component_patterns.reserve(_num_comps);
	for (size_t i=0; i<_num_comps; i++)
	{
		Handle h(createPatternLink(_component_vars[i], _varlist._simple_typemap,
		                           _components[i], _pat.optionals));
		_component_patterns.emplace_back(h);
	}
}

void PatternLink::init(void)
{
	_pat.redex_name = "anonymous PatternLink";
	extract_variables(_outgoing);

	if (2 < _outgoing.size() or
	   (2 == _outgoing.size() and _outgoing[1] != _body))
	{
		throw InvalidParamException(TRACE_INFO,
		      "Expecting (optional) variable decls and a body; got %s",
		      toString().c_str());
	}

	unbundle_clauses(_body);
	common_init();
	setup_components();
}

/* ================================================================= */

/// Special constructor used during just-in-time pattern compilation.
///
/// It assumes that the variables have already been correctly extracted
/// from the body, as appropriate.
PatternLink::PatternLink(const Variables& vars, const Handle& body)
	: ScopeLink(PATTERN_LINK, HandleSeq())
{
	_pat.redex_name = "jit PatternLink";

	_varlist = vars;
	_body = body;
	unbundle_clauses(_body);
	common_init();
	setup_components();
}

/* ================================================================= */

/// Special constructor used only to make single concrete pattern
/// components.  We are given the pre-computed components; we only
/// have to store them.
PatternLink::PatternLink(const std::set<Handle>& vars,
                         const VariableTypeMap& typemap,
                         const HandleSeq& compo,
                         const std::set<Handle>& opts)
	: ScopeLink(PATTERN_LINK, HandleSeq())
{
	// First, lets deal with the vars. We have discarded the original
	// order of the variables, and I think that's OK, because we will
	// never have the substitute aka beta-redex aka putlink method
	// called on us, not directly, at least.  If we need it, then the
	// API will need to be changed...
	// So all we need is the varset, and the subset of the typemap.
	_varlist.varset = vars;
	_varlist.varseq.clear();
	for (const Handle& v : vars)
	{
		_varlist.varseq.emplace_back(v);
		auto it = typemap.find(v);
		if (it != typemap.end())
			_varlist._simple_typemap.insert(*it);
	}

	// Next, the body... there's no _body for lambda. The compo is the
	// cnf_clauses; we have to reconstruct the optionals.  We cannot
	// use extract_optionals because opts have been stripped already.

	_pat.cnf_clauses = compo;
	for (const Handle& h : compo)
	{
		bool h_is_opt = false;
		for (const Handle& opt : opts)
		{
			if (is_atom_in_tree(opt, h))
			{
				_pat.optionals.insert(opt);
				_pat.clauses.emplace_back(opt);
				h_is_opt = true;
				break;
			}
		}
		if (not h_is_opt)
		{
			_pat.clauses.emplace_back(h);
			_pat.mandatory.emplace_back(h);
		}
	}
	locate_defines(_pat.clauses);
	locate_globs(_pat.clauses);

	// The rest is easy: the evaluatables and the connection map
	unbundle_virtual(_varlist.varset, _pat.cnf_clauses,
	                 _fixed, _virtual, _pat.black);
	_num_virts = _virtual.size();
	OC_ASSERT (0 == _num_virts, "Must not have any virtuals!");

	_components.emplace_back(compo);
	_num_comps = 1;

	make_connectivity_map(_pat.cnf_clauses);
	_pat.redex_name = "Unpacked component of a virtual link";

	make_term_trees();
}

/* ================================================================= */

/// Constructor that takes a pre-determined set of variables, and
/// a list of clauses to solve.  This is currently kind-of crippled,
/// since no variable type restricions are possible, and no optionals,
/// either.  This is used only for backwards-compatibility API's.
PatternLink::PatternLink(const std::set<Handle>& vars,
                         const HandleSeq& clauses)
	: ScopeLink(PATTERN_LINK, HandleSeq())
{
	_varlist.varset = vars;
	_pat.clauses = clauses;
	common_init();
	setup_components();
}

/* ================================================================= */

PatternLink::PatternLink(const HandleSeq& hseq,
                         TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(PATTERN_LINK, hseq, tv, av)
{
	init();
}

PatternLink::PatternLink(const Handle& body,
                         TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(PATTERN_LINK, HandleSeq({body}), tv, av)
{
	init();
}

PatternLink::PatternLink(const Handle& vars, const Handle& body,
                         TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(PATTERN_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

PatternLink::PatternLink(Type t, const HandleSeq& hseq,
                         TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(t, hseq, tv, av)
{
	// BindLink has other init sequences
	if (BIND_LINK == t) return;
	init();
}

PatternLink::PatternLink(Link &l)
	: ScopeLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PATTERN_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PatternLink, got %s", tname.c_str());
	}

	// BindLink has other init sequences
	if (BIND_LINK == tscope) return;

	init();
}


/* ================================================================= */
///
/// Unpack the clauses.
///
/// The predicate is either an AndLink of clauses to be satisfied, or a
/// single clause. Other link types, such as OrLink and SequentialAnd,
/// are treated here as single clauses; unpacking them here would lead
/// to confusion in the pattern matcher.  This is partly because, after
/// unpacking, clauses can be grounded in an arbitrary order; thus,
/// SequentialAnd's must be grounded and evaluated sequentially, and
/// thus, not unpacked.  In the case of OrLinks, there is no flag to
/// say that "these are disjoined", so again, that has to happen later.
void PatternLink::unbundle_clauses(const Handle& hbody)
{
	Type t = hbody->getType();
	// For just right now, unpack PresentLink, although that is not
	// technically correct in the long-run. XXX FIXME In the long run,
	// nothing should be unpacked, since everything should be run-time
	// evaluatable. i.e. everything should be a predicate, and that's
	// that. ??? But how can this be done? We have to pattern-match
	// disjoint clauses, connected via variables only; how else can this
	// be done, except by unpacking?  I'm confused.
	//
	// For SequentialAndLinks, which are expected to be evaluated
	// in-order, we need to fish out any PresentLinks, and add them
	// to the list of clauses to be grounded.  Of course, the
	// SequentialAndLink itself also has to be evaluated, so we add it
	// too.
	_pat.body = hbody;
	if (PRESENT_LINK == t)
	{
		_pat.clauses = LinkCast(hbody)->getOutgoingSet();
	}
	else if (AND_LINK == t)
	{
		const HandleSeq& oset = LinkCast(hbody)->getOutgoingSet();
		for (const Handle& ho : oset)
		{
			Type ot = ho->getType();
			// If there is a PresentLink hiding under the AndLink
			// then pull clauses out of it.
			if (PRESENT_LINK == ot)
			{
				const HandleSeq& pset = LinkCast(ho)->getOutgoingSet();
				for (const Handle& ph : pset)
					_pat.clauses.emplace_back(ph);
			}
			else
				_pat.clauses.emplace_back(ho);
		}
	}
	else if (SEQUENTIAL_AND_LINK == t or SEQUENTIAL_OR_LINK == t)
	{
		// XXX FIXME, Just like in trace_connectives, assume we are
		// working with the DefaultPatternMatchCB, which uses these.
		std::set<Type> connectives({AND_LINK, SEQUENTIAL_AND_LINK,
		                            OR_LINK, SEQUENTIAL_OR_LINK, NOT_LINK});
		const HandleSeq& oset = LinkCast(hbody)->getOutgoingSet();
		unbundle_clauses_rec(connectives, oset);

		_pat.clauses.emplace_back(hbody);
	}
	else
	{
		// There's just one single clause!
		_pat.clauses.emplace_back(hbody);
	}
}

/// Search for any PRESENT_LINK or ABSENT_LINK's that are
/// recusively embedded inside some evaluatable clause.  Expose these
/// as first-class, groundable clauses.
void PatternLink::unbundle_clauses_rec(const std::set<Type>& connectives,
                                       const HandleSeq& nest)
{
	for (const Handle& ho : nest)
	{
		Type ot = ho->getType();
		if (PRESENT_LINK == ot)
		{
			const HandleSeq& pset = LinkCast(ho)->getOutgoingSet();
			for (const Handle& ph : pset)
				_pat.clauses.emplace_back(ph);
		}
		else if (ABSENT_LINK == ot)
		{
			LinkPtr lopt(LinkCast(ho));

			// We insist on an arity of 1, because anything else is
			// ambiguous: consider absent(A B) is that: "both A and B must
			// be absent"?  Or is it "if any of A and B are absent, then .."
			if (1 != lopt->getArity())
				throw InvalidParamException(TRACE_INFO,
					"AbsentLink can have an arity of one only!");

			const Handle& inv(lopt->getOutgoingAtom(0));
			_pat.optionals.insert(inv);
			_pat.cnf_clauses.emplace_back(inv);
		}
		else if (connectives.find(ot) != connectives.end())
		{
			LinkPtr lnest(LinkCast(ho));
			unbundle_clauses_rec(connectives, lnest->getOutgoingSet());
		}
	}
}

void PatternLink::locate_defines(HandleSeq& clauses)
{
	for (const Handle& clause: clauses)
	{
		FindAtoms fdpn(DEFINED_PREDICATE_NODE, DEFINED_SCHEMA_NODE, true);
		fdpn.stopset.insert(SCOPE_LINK);
		fdpn.search_set(clause);

		for (const Handle& sh : fdpn.varset)
		{
			_pat.defined_terms.insert(sh);
		}
	}
}

void PatternLink::locate_globs(HandleSeq& clauses)
{
	for (const Handle& clause: clauses)
	{
		FindAtoms fgn(GLOB_NODE, true);
		fgn.search_set(clause);

		for (const Handle& sh : fgn.least_holders)
		{
			_pat.globby_terms.insert(sh);
		}
	}
}

/* ================================================================= */
/**
 * A simple validatation a collection of clauses for correctness.
 *
 * Every clause should contain at least one variable in it; clauses
 * that are constants and can be trivially discarded.
 */
void PatternLink::validate_clauses(std::set<Handle>& vars,
                                   HandleSeq& clauses,
                                   HandleSeq& constants)

{
	// The Fuzzy matcher does some strange things: it declares no
	// vars at all, only clauses, and then uses the pattern matcher
	// to automatically explore nearby atoms. As a result, all of
	// its clauses are "constant", and we allow this special case.
	// Need to review the rationality of this design...
	if (0 < vars.size())
	{
		// Make sure that the user did not pass in bogus clauses.
		// Make sure that every clause contains at least one variable.
		// The presence of constant clauses will mess up the current
		// pattern matcher.  Constant clauses are "trivial" to match,
		// and so its pointless to even send them through the system.
		bool bogus = remove_constants(vars, clauses, constants);
		if (bogus)
		{
			logger().warn("%s: Constant clauses removed from pattern",
			              __FUNCTION__);
		}
	}

	// Make sure that each declared variable appears in some clause.
	// We won't (can't) ground variables that don't show up in a
	// clause.  They are presumably there due to programmer error.
	// Quoted variables are constants, and so don't count.
	for (const Handle& v : vars)
	{
		if (not is_unquoted_in_any_tree(clauses, v))
		{
			vars.erase(v);
			throw InvalidParamException(TRACE_INFO,
			   "The variable %s does not appear (unquoted) in any clause!",
			   v->toShortString().c_str());
		}
	}

	// The above 1-2 combination of removing constant clauses, and
	// removing variables, can result in an empty body. That surely
	// warrants a throw, and BuggyQuoteUTest is expecting one.
	if (0 == vars.size() and 0 == clauses.size())
			throw InvalidParamException(TRACE_INFO,
			   "No variable appears (unquoted) anywhere in any clause!");
}

/* ================================================================= */
/**
 * Given the initial list of variables and clauses, separate these into
 * the mandatory, optional and fuzzy clauses.
 */
void PatternLink::extract_optionals(const std::set<Handle> &vars,
                                    const std::vector<Handle> &component)
{
	// Split in positive and negative clauses
	for (const Handle& h : component)
	{
		Type t = h->getType();
		if (ABSENT_LINK == t)
		{
			LinkPtr lopt(LinkCast(h));

			// We insist on an arity of 1, because anything else is
			// ambiguous: consider absent(A B) is that: "both A and B must
			// be absent"?  Or is it "if any of A and B are absent, then .."
			if (1 != lopt->getArity())
				throw InvalidParamException(TRACE_INFO,
					"AbsentLink can have an arity of one only!");

			const Handle& inv(lopt->getOutgoingAtom(0));
			_pat.optionals.insert(inv);
			_pat.cnf_clauses.emplace_back(inv);
		}
		else
		{
			_pat.mandatory.emplace_back(h);
			_pat.cnf_clauses.emplace_back(h);
		}
	}
}

/* ================================================================= */

/* utility -- every variable in the key term will get the value. */
static void add_to_map(std::unordered_multimap<Handle, Handle>& map,
                       const Handle& key, const Handle& value)
{
	if (key->getType() == VARIABLE_NODE) map.insert({key, value});
	LinkPtr lll(LinkCast(key));
	if (NULL == lll) return;
	const HandleSeq& oset = lll->getOutgoingSet();
	for (const Handle& ho : oset) add_to_map(map, ho, value);
}

/// Sort out the list of clauses into four classes:
/// virtual, evaluatable, executable and concrete.
///
/// A term is "evalutable" if it contains a GroundedPredicateNode,
/// or if it inherits from VirtualLink (such as the GreaterThanLink).
/// Such terms need evaluation at grounding time, to determine
/// thier truth values.
///
/// A term may also be evaluatable if consists of connectives (such as
/// AndLink, OrLink, NotLink) used to join together evaluatable terms.
/// Normally, the above search for GPN's and VirtualLinks should be
/// enough, except when an entire term is a variable.  Thus, for
/// example, a term such as (NotLink (VariableNode $x)) needs to be
/// evaluated at grounding-time, even though it does not currently
/// contain a GPN or a VirtualLink: the grounding might contain it;
/// We don't know yet.  However, there's a gotcha: we don't yet know
/// what the connectives are. The actual connectives depend on the
/// callback; the default callback uses AndLink, OrLink, NotLink, but
/// other callbacks may pick something else.  Thus, we cannot do this
/// here. By contrast, GetLink, SatisfactionLink and BindLink
/// explicitly assume the default callback, so they do the additional
/// unbundling there.
///
/// A term is "executable" if it is an ExecutionOutputLink
/// or if it inherits from one (such as PlusLink, TimesLink).
/// Such terms need execution at grounding time, to determine
/// thier actual form.  Note that executable terms may frequently
/// occur underneath evaluatable terms, e.g. if something is greater
/// than the sum of two other things.
///
/// A clause is "virtual" if it has an evaluatable term inside of it,
/// and that term has two or more variables in it. Such virtual
/// clauses not only require evaluation, but an evaluation must be
/// performed for each different variable grounding.  Virtual clauses
/// get a very different (and more complex) treatment from the pattern
/// matcher.
///
/// Virtual cluases are hard for the pattern matcher in two different
/// ways: first, any variables in them must be grounded before they can
/// be evaluated, and that grounding has to occur *before* evaluation.
/// Thus, non-virtual clasues must be grounded first. Another problem
/// arises when a virtual clause has two or more variables in it, and
/// those variables are grounded by different disconnected graph
/// components; the combinatoric explosion has to be handled...
///
void PatternLink::unbundle_virtual(const std::set<Handle>& vars,
                                   const HandleSeq& clauses,
                                   HandleSeq& fixed_clauses,
                                   HandleSeq& virtual_clauses,
                                   std::set<Handle>& black_clauses)
{
	for (const Handle& clause: clauses)
	{
		bool is_virtual = false;
		bool is_black = false;

#ifdef BROKEN_DOESNT_WORK
// The below should have worked to set things up, but it doesn't,
// and I'm too lazy to investigate, because an alternate hack is
// working, at the moment.
		// If a clause is a variable, we have to make the worst-case
		// assumption that it is evaluatable, so that we can evaluate
		// it later.
		if (VARIABLE_NODE == clause->getType())
		{
			_pat.evaluatable_terms.insert(clause);
			add_to_map(_pat.in_evaluatable, clause, clause);
			is_black = true;
		}
#endif

		FindAtoms fgpn(GROUNDED_PREDICATE_NODE, true);
		fgpn.stopset.insert(SCOPE_LINK);
		fgpn.search_set(clause);
		for (const Handle& sh : fgpn.least_holders)
		{
			_pat.evaluatable_terms.insert(sh);
			add_to_map(_pat.in_evaluatable, sh, sh);
			// But they're virtual only if they have two or more
			// unquoted, bound variables in them. Otherwise, they
			// can be evaluated on the spot.
			if (2 <= num_unquoted_in_tree(sh, vars))
			{
				is_virtual = true;
				is_black = true;
			}
		}
		for (const Handle& sh : fgpn.holders)
			_pat.evaluatable_holders.insert(sh);

		// Subclasses of VirtualLink, e.g. GreaterThanLink, which
		// are essentially a kind of EvaluationLink holding a GPN
		FindAtoms fgtl(VIRTUAL_LINK, true);
		fgtl.search_set(clause);
		// Unlike the above, its varset, not least_holders...
		// because its a link...
		for (const Handle& sh : fgtl.varset)
		{
			_pat.evaluatable_terms.insert(sh);
			_pat.evaluatable_holders.insert(sh);
			add_to_map(_pat.in_evaluatable, sh, sh);
			// But they're virtual only if they have two or more
			// unquoted, bound variables in them. Otherwise, they
			// can be evaluated on the spot. Virtuals are not black.
			if (2 <= num_unquoted_in_tree(sh, vars))
				is_virtual = true;
		}
		for (const Handle& sh : fgtl.holders)
			_pat.evaluatable_holders.insert(sh);

		// Subclasses of FunctionLink, e.g. ExecutionOutputLink,
		// but also PlusLink, TimesLink are all executable. They
		// need to be executed *before* pattern matching, but after
		// any variables in them have been grounded, so that the
		// pattern match is performed on the result of the execution.
		// Thus, these are treated by the same virtual-graph algo as
		// the virtual links.
		FindAtoms feol(FUNCTION_LINK, true);
		feol.search_set(clause);

		for (const Handle& sh : feol.varset)
		{
			_pat.executable_terms.insert(sh);
			_pat.executable_holders.insert(sh);
			add_to_map(_pat.in_executable, sh, sh);
			// But they're virtual only if they have two or more
			// unquoted, bound variables in them. Otherwise, they
			// can be evaluated on the spot.
			if (2 <= num_unquoted_in_tree(sh, vars))
			{
				is_virtual = true;
				is_black = true;
			}
		}
		for (const Handle& sh : feol.holders)
			_pat.executable_holders.insert(sh);

		if (is_virtual)
			virtual_clauses.emplace_back(clause);
		else
			fixed_clauses.emplace_back(clause);

		if (is_black)
			black_clauses.insert(clause);
	}
}

/* ================================================================= */

/// Starting from the top of a clause, trace down through the tree
/// of connectives.  If a term appears under a connective, and there
/// is a path of connectives all the way to the top, then we have to
/// assume the term is evaluatable, as the whole point of connectives
/// to to connect evaluatable terms.  Thus, for example, for a clause
/// having the form (AndLink stuff (OrLink more-stuff (NotLink not-stuff)))
/// we have to assume that stuff, more-stuff and not-stuff are all
/// evaluatable.
void PatternLink::trace_connectives(const std::set<Type>& connectives,
                                    const HandleSeq& oset,
                                    int quotation_level)
{
	for (const Handle& term: oset)
	{
		Type t = term->getType();

		// Deal with quote
		if (t == QUOTE_LINK)
			quotation_level++;
		else if (t == UNQUOTE_LINK)
			quotation_level--;

		if (quotation_level > 0
		    or connectives.find(t) == connectives.end()) continue;
		_pat.evaluatable_holders.insert(term);
		add_to_map(_pat.in_evaluatable, term, term);
		LinkPtr lp(LinkCast(term));
		if (lp)
			trace_connectives(connectives, lp->getOutgoingSet(),
			                  quotation_level);
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
 * clause. Perhaps this could be simplied somehow ...
 */
void PatternLink::make_connectivity_map(const HandleSeq& component)
{
	for (const Handle& h : _pat.cnf_clauses)
	{
		make_map_recursive(h, h);
	}

	// Save some minor amount of space by erasing those atoms that
	// participate in only one clause. These atoms cannot be used
	// to determine connectivity between clauses, and so are un-needed.
	auto it = _pat.connectivity_map.begin();
	auto end = _pat.connectivity_map.end();
	while (it != end)
	{
		if (it->second.size() == 1)
			it = _pat.connectivity_map.erase(it);
		else
			it++;
	}
}

void PatternLink::make_map_recursive(const Handle& root, const Handle& h)
{
	_pat.connectivity_map[h].emplace_back(root);

	LinkPtr l(LinkCast(h));
	if (l)
	{
		for (const Handle& ho: l->getOutgoingSet())
			make_map_recursive(root, ho);
	}
}

/// Make sure that every variable appears in some groundable clause.
/// Variables have to be grounded before an evaluatable clause
/// containing them can be evaluated.  If they can never be grounded,
/// then any clauses in which they appear cannot ever be evaluated,
/// leading to an undefined condition.  So, explicitly check and throw
/// an error if a pattern is ill-formed.
void PatternLink::check_satisfiability(const std::set<Handle>& vars,
                                       const std::vector<std::set<Handle>>& compvars)
{
	// Compute the set-union of all component vars.
	std::set<Handle> vunion;
	for (const std::set<Handle>& vset : compvars)
	{
		for (const Handle& v : vset)
			vunion.insert(v);
	}

	// Is every variable in some component? If not, then throw.
	for (const Handle& v : vars)
	{
		auto it = vunion.find(v);
		if (vunion.end() == it)
		{
			throw InvalidParamException(TRACE_INFO,
				"Variable not groundable: %s\n", v->toString().c_str());
		}
	}
}

// Hack alert: Definitely it should not be here. Though some refactoring
// regarding shared libraries circular dependencies (liblambda and libquery)
// need to be done before fixing...
const PatternTermPtr PatternTerm::UNDEFINED(std::make_shared<PatternTerm>());

void PatternLink::make_term_trees()
{
	for (const Handle& clause : _pat.cnf_clauses)
	{
		PatternTermPtr root_term(std::make_shared<PatternTerm>());
		make_term_tree_recursive(clause, clause, root_term);
	}
}

void PatternLink::make_term_tree_recursive(const Handle& root,
                                           Handle h,
                                           PatternTermPtr& parent)
{
	Type t = h->getType();

	// Ignore quoting and unquoting nodes in the PatternTerm
	if ((not parent->isQuoted() and QUOTE_LINK == t)
	    or (parent->getQuotationLevel() == 1 and UNQUOTE_LINK == t)) {
		LinkPtr lp(LinkCast(h));
		if (1 != lp->getArity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink has "
			                            "unexpected arity!");
		h = lp->getOutgoingAtom(0);
	}

	PatternTermPtr ptm(std::make_shared<PatternTerm>(parent, h));
	parent->addOutgoingTerm(ptm);
	_pat.connected_terms_map[{h, root}].emplace_back(ptm);

	// Update the PatternTerm quotation level
	if (QUOTE_LINK == t)
		ptm->addQuote();
	else if (UNQUOTE_LINK == t)
		ptm->remQuote();

	// If the current node is a bound variable store this information for
	// later checks. The flag telling whether the term subtree contains
	// any bound variable is set by addBoundVariable() method for all terms
	// on the path up to the root (unless it has been set already).
	t = h->getType();
	if ((VARIABLE_NODE == t or GLOB_NODE == t)
	    and not ptm->isQuoted()
	    and _varlist.varset.end() != _varlist.varset.find(h))
	{
		ptm->addBoundVariable();
		return;
	}

	LinkPtr l(LinkCast(h));
	if (l)
		for (const Handle& ho: l->getOutgoingSet())
			make_term_tree_recursive(root, ho, ptm);
}

/* ================================================================= */
/**
 * Check that all clauses are connected
 */
void PatternLink::check_connectivity(const std::vector<HandleSeq>& components)
{
	if (1 == components.size()) return;

	// Users are going to be stumped by this one, so print
	// out a verbose, user-freindly debug message to help
	// them out.
	std::stringstream ss;
	ss << "Pattern is not connected! Found "
	   << components.size() << " components:\n";
	int cnt = 1;
	for (const auto& comp : components)
	{
		ss << "Connected component " << cnt
		   << " consists of ----------------: \n";
		for (Handle h : comp) ss << h->toString();
		cnt++;
	}
	throw InvalidParamException(TRACE_INFO, ss.str().c_str());
}

/* ================================================================= */

void PatternLink::debug_log(void) const
{
	if (!logger().isFineEnabled())
		return;

	// Log the predicate ...
	logger().fine("Pattern '%s' has following clauses:",
	              _pat.redex_name.c_str());
	int cl = 0;
	for (const Handle& h : _pat.mandatory)
	{
		std::stringstream ss;
		ss << "Mandatory " << cl << ":";
		if (_pat.evaluatable_holders.find(h) != _pat.evaluatable_holders.end())
			ss << " (evaluatable)";
		if (_pat.executable_holders.find(h) != _pat.executable_holders.end())
			ss << " (executable)";
		ss << std::endl;
		ss << h->toShortString();
		logger().fine() << ss.str();
		cl++;
	}

	if (0 < _pat.optionals.size())
	{
		logger().fine("Predicate includes the following optional clauses:");
		cl = 0;
		for (const Handle& h : _pat.optionals)
		{
			std::stringstream ss;
			ss << "Optional clause " << cl << ":";
			if (_pat.evaluatable_holders.find(h) != _pat.evaluatable_holders.end())
				ss << " (evaluatable)";
			if (_pat.executable_holders.find(h) != _pat.executable_holders.end())
				ss << " (executable)";
			ss << std::endl;
			ss << h->toShortString();
			logger().fine() << ss.str();
			cl++;
		}
	}
	else
		logger().fine("No optional clauses");

	// Print out the bound variables in the predicate.
	for (const Handle& h : _varlist.varset)
	{
		if (NodeCast(h))
			logger().fine() << "Bound var: " << h->toShortString();
	}

	if (_varlist.varset.empty())
		logger().fine("There are no bound vars in this pattern");
}

/* ===================== END OF FILE ===================== */
