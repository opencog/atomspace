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
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomspace/Node.h>
#include <opencog/atoms/reduct/FreeLink.h>

#include "PatternLink.h"
#include "PatternUtils.h"
#include "VariableList.h"

using namespace opencog;

void PatternLink::common_init(void)
{
	validate_clauses(_varlist.varset, _pat.clauses);
	extract_optionals(_varlist.varset, _pat.clauses);

	// Locate the black-box clauses.
	unbundle_virtual(_varlist.varset, _pat.cnf_clauses,
	                 _fixed, _virtual, _pat.black);
	_num_virts = _virtual.size();

	// unbundle_virtual does not handle connectives. Here, we assume that
	// we are being run with the DefaultPatternMatchCB, and so we assume
	// that the logical connectives are AndLink, OrLink and NotLink.
	// Tweak the evaluatable_holders to reflect this.
	std::set<Type> connectives({AND_LINK, OR_LINK, NOT_LINK});
	trace_connectives(connectives, _pat.clauses);

	// Split the non-virtual clauses into connected components
	get_connected_components(_varlist.varset, _fixed,
	                         _components, _component_vars);
	_num_comps = _components.size();
}

void PatternLink::init(void)
{
	_pat.redex_name = "anonymous PatternLink";
	extract_variables(_outgoing);
	unbundle_clauses(_body);
	common_init();
}

// Special ctor for use by SatisfactionLink; we are given
// the pre-computed components.
PatternLink::PatternLink(const std::set<Handle>& vars,
                         const VariableTypeMap& typemap,
                         const HandleSeq& compo,
                         const std::set<Handle>& opts)
	: Link(PATTERN_LINK, HandleSeq())
{
	// First, lets deal with the vars. We have discarded the original
	// order of the variables, and I think that's OK, because we will
	// not be using the substitute method, I don't think. If we need it,
	// then the API will need to be changed...
	// So all we need is the varset, and the subset of the typemap.
	_varlist.varset = vars;
	for (const Handle& v : vars)
	{
		auto it = typemap.find(v);
		if (it != typemap.end())
			_varlist.typemap.insert(*it);
	}

	// Next, the body... there no _body for lambda. The compo is the
	// _cnf_clauses; we have to reconstruct the optionals.  We cannot
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
				_pat.clauses.push_back(opt);
				h_is_opt = true;
				break;
			}
		}
		if (not h_is_opt)
			_pat.mandatory.push_back(h);
	}

	// The rest is easy: the evaluatables and the connection map
	HandleSeq concs, virts;
	unbundle_virtual(_varlist.varset, _pat.cnf_clauses,
	                 concs, virts, _pat.black);
	make_connectivity_map(_pat.cnf_clauses);
	_pat.redex_name = "Unpacked component of a virtual link";
}

PatternLink::PatternLink(const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: Link(PATTERN_LINK, hseq, tv, av)
{
	init();
}

PatternLink::PatternLink(const Handle& vars, const Handle& body,
                   TruthValuePtr tv, AttentionValuePtr av)
	: Link(PATTERN_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

PatternLink::PatternLink(Type t, const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, hseq, tv, av)
{
	// Derived link-types have other init sequences
	if (PATTERN_LINK != t) return;
	init();
}

PatternLink::PatternLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PATTERN_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PatternLink, got %s", tname.c_str());
	}

	// Derived link-types have other init sequences
	if (PATTERN_LINK != tscope) return;

	init();
}


/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
void PatternLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = oset.size();
	if (2 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at most two, got %d", sz);

	// If the outgoing set size is one, then there are no variable
	// declarations; extract all free variables.
	if (1 == sz)
	{
		_body = oset[0];

		// Use the FreeLink class to find all the variables;
		// Use the VariableList class for build the Variables struct.
		FreeLink fl(oset[0]);
		VariableList vl(fl.get_vars());
		_varlist = vl.get_variables();
		return;
	}

	// If we are here, then the first outgoing set member should be
	// a variable declaration.
	_body = oset[1];

	// Either the first element is a VariableList, or its a naked
	// variable, or its a typed variable.  Use the VariableList
	// class as a tool to extract the variables for us.
	Type t = oset[0]->getType();
	if (VARIABLE_LIST == t)
	{
		VariableList vl(LinkCast(oset[0])->getOutgoingSet());
		_varlist = vl.get_variables();
	}
	else
	{
		HandleSeq v;
		v.push_back(oset[0]);
		VariableList vl(v);
		_varlist = vl.get_variables();
	}
}

/* ================================================================= */
///
/// Unpack the clauses.
///
/// The predicate is either an AndLink of clauses to be satisfied, or a
/// single clause. Other link types, such as OrLink and SequentialAnd,
/// are treated here as single clauses; unpacking them here would lead
/// to confusion in the pattern matcher. This is patly because, after
/// unpacking, clauses can be grounded in  an arbitrary order; thus,
/// SequentialAnd's must not be unpacked. In the case of OrLinks, there
/// is no flag to say that "these are disjoined", so again, that has to
/// happen later.
void PatternLink::unbundle_clauses(const Handle& hbody)
{
	Type t = hbody->getType();
	if (AND_LINK == t)
	{
		_pat.clauses = LinkCast(hbody)->getOutgoingSet();
	}
	else
	{
		// There's just one single clause!
		_pat.clauses.push_back(hbody);
	}
}

/* ================================================================= */
/**
 * A simple validatation a collection of clauses for correctness.
 *
 * Every clause should contain at least one variable in it; clauses
 * that are constants and can be trivially discarded.
 *
XXX WTF, what about evaluatables? How did those not get removed???
I don't get it... fixme
 */
void PatternLink::validate_clauses(std::set<Handle>& vars,
                                    HandleSeq& clauses)

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
		bool bogus = remove_constants(vars, clauses);
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
	//
	// XXX Well, we could throw, here, but sureal gives us spurious
	// variables, so instead of throwing, we just discard them and
	// print a warning.
	for (const Handle& v : vars)
	{
		if (not is_unquoted_in_any_tree(clauses, v))
		{
/*
			logger().warn(
				"%s: The variable %s does not appear (unquoted) in any clause!",
			           __FUNCTION__, v->toShortString().c_str());
*/
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
 * the mandatory and optional clauses.
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
			// ambiguous: consider not(A B) is that (not(A) and not(B))
			// or is it (not(A) or not(B))?
			if (1 != lopt->getArity())
				throw InvalidParamException(TRACE_INFO,
					"NotLink and AbsentLink can have an arity of one only!");

			const Handle& inv(lopt->getOutgoingAtom(0));
			_pat.optionals.insert(inv);
			_pat.cnf_clauses.push_back(inv);
		}
		else
		{
			_pat.mandatory.push_back(h);
			_pat.cnf_clauses.push_back(h);
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
/// A term is "evalutable" if it contains a GroundedPredicate,
/// or if it inherits from VirtualLink (such as the GreaterThanLink).
/// Such terms need evaluation at grounding time, to determine
/// thier truth values.
///
/// A term may also be evaluatable if consists of connectives (such as
/// AndLink, OrLink, NotLink) used to join together evaluatable terms.
/// Normally, the above search for GPN's and VirtualLinks should be
/// enough, expect when an entire term is a variable.  Thus, for
/// example, a term such as (NotLink (VariableNode $x)) needs to be
/// evaluated at grounding-time, even though it does not currently
/// contain a GPN or a VirtualLink: the grounding might contain it;
/// We don't know yet.  However, there's a gotcha: we don't yet know
/// what the connectives are. The actual connectives depend on the
/// callback; the default callback uses AndLink, OrLink, NotLink, but
/// other callbacks may pick something else.  Thus, we cannot do this
/// here. By contrast, SatisfactionLink and BindLink explicitly assume
/// the default callback, so they do the additional unbundling there.
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
/// components; the combinatoric explosino has to be handled...
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

#ifdef BORKEN_DOESNT_WORK
// The below should have worked to set things up, but it doesn't,
// and I'm too lazy to investigate, because an alternate hack is
// working, at the moment.
		// If a clause is a variable, we have to make the worst-case
		// assumption that it is evaulatable, so that we can evaluate
		// it later.
		if (VARIABLE_NODE == clause->getType())
		{
			_pat.evaluatable_terms.insert(clause);
			add_to_map(_pat.in_evaluatable, clause, clause);
			is_black = true;
		}
#endif

		FindAtoms fgpn(GROUNDED_PREDICATE_NODE, true);
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

		// Subclasses of ExecutionOutputLink, e.g. PlusLink,
		// TimesLink are executable. They get treated by the
		// same virtual-graph algo as the virtual links.
		FindAtoms feol(EXECUTION_OUTPUT_LINK, true);
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
			virtual_clauses.push_back(clause);
		else
			fixed_clauses.push_back(clause);

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
                                     const HandleSeq& oset)
{
	for (const Handle& term: oset)
	{
		Type t = term->getType();
		if (connectives.find(t) == connectives.end()) continue;
		_pat.evaluatable_holders.insert(term);
		add_to_map(_pat.in_evaluatable, term, term);
		LinkPtr lp(LinkCast(term));
		if (lp)
			trace_connectives(connectives, lp->getOutgoingSet());
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
	_pat.connectivity_map[h].push_back(root);

	LinkPtr l(LinkCast(h));
	if (l)
	{
		for (const Handle& ho: l->getOutgoingSet())
			make_map_recursive(root, ho);
	}
}

/* ================================================================= */

void PatternLink::debug_print(void) const
{
	// Print out the predicate ...
	printf("\nRedex '%s' has following clauses:\n",
	       _pat.redex_name.c_str());
	int cl = 0;
	for (const Handle& h : _pat.mandatory)
	{
		printf("Mandatory %d:", cl);
		if (_pat.evaluatable_holders.find(h) != _pat.evaluatable_holders.end())
			printf(" (evaluatable)");
		if (_pat.executable_holders.find(h) != _pat.executable_holders.end())
			printf(" (executable)");
		printf("\n");
		prt(h);
		cl++;
	}

	if (0 < _pat.optionals.size())
	{
		printf("Predicate includes the following optional clauses:\n");
		cl = 0;
		for (const Handle& h : _pat.optionals)
		{
			printf("Optional clause %d:", cl);
			if (_pat.evaluatable_holders.find(h) != _pat.evaluatable_holders.end())
				printf(" (evaluatable)");
			if (_pat.executable_holders.find(h) != _pat.executable_holders.end())
				printf(" (executable)");
			printf("\n");
			prt(h);
			cl++;
		}
	}
	else
		printf("No optional clauses\n");

	// Print out the bound variables in the predicate.
	for (const Handle& h : _varlist.varset)
	{
		if (NodeCast(h))
			printf("Bound var: "); prt(h);
	}

	if (_varlist.varset.empty())
		printf("There are no bound vars in this pattern\n");

	printf("\n");
	fflush(stdout);
}

/* ===================== END OF FILE ===================== */
