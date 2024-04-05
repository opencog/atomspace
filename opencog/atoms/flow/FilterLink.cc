/*
 * opencog/atoms/flow/FilterLink.cc
 *
 * Copyright (C) 2015, 2016, 2022 Linas Vepstas
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/core/VariableSet.h>
#include <opencog/atoms/rule/RuleLink.h>
#include <opencog/atoms/value/LinkValue.h>

#include "FilterLink.h"

using namespace opencog;

void FilterLink::init(void)
{
	// Filters consist of a function, and the data to apply the
	// function to.  The function can be explicit (inheriting from
	// ScopeLink) or implicit (we automatically fish out free variables).
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"FilterLink is expected to be arity-2 only!");

	Handle termpat = _outgoing[0];
	Type tscope = termpat->get_type();

	// Expand definitions
	if (nameserver().isA(tscope, DEFINED_PROCEDURE_NODE))
	{
		termpat = DefineLink::get_definition(termpat);
		if (nullptr == termpat)
			throw SyntaxException(TRACE_INFO,
				"FilterLink cannot find defnition for %s",
				_outgoing[0]->to_string().c_str());

		tscope = termpat->get_type();
	}

	// First argument must be a function of some kind.  All functions
	// are specified using a ScopeLink, to bind the input-variables.
	if (nameserver().isA(tscope, SCOPE_LINK))
	{
		_pattern = ScopeLinkCast(termpat);
	}
	else
	{
		const Handle& body = termpat;
		FreeVariables fv;
		fv.find_variables(body);
		Handle decl(createVariableSet(std::move(fv.varseq)));
		_pattern = createScopeLink(std::move(decl), body);
	}
	_mvars = &_pattern->get_variables();
	_varset = &_mvars->varset;

	// RuleLinks are a special type of ScopeLink.  They specify a
	// re-write that should be performed.  Viz, RuleLinks are
	// of the form P(x)->Q(x).  Here, the `_rewrite` is the Q(x)
	if (nameserver().isA(tscope, RULE_LINK))
		_rewrite = RuleLinkCast(_pattern)->get_implicand();

	// Locate all GlobNodes in the pattern
	FindAtoms fgn(GLOB_NODE, true);
	fgn.search_set(_pattern->get_body());
	for (const Handle& sh : fgn.least_holders)
		_globby_terms.insert(sh);

	// FunctionLink::init() will extract free variables. But we don't
	// really need this for anything, so don't run it.
	// FunctionLink::init();
}

FilterLink::FilterLink(const Handle& pattern, const Handle& term)
	: FunctionLink(HandleSeq({pattern, term}), FILTER_LINK)
{
	init();
}

FilterLink::FilterLink(Type t, const Handle& body)
	: FunctionLink(HandleSeq({body}), t)
{
	// Derived types have a different initialization sequence.
	if (FILTER_LINK != t) return;
	init();
}

FilterLink::FilterLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, FILTER_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
			"Expecting a FilterLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (FILTER_LINK != t) return;
	init();
}

// ===============================================================

/// Direct side-by-side compare, for VECT being either
/// std::vector<Handle> or std::vector<Value>
template<typename VECT>
bool FilterLink::glob_compare(const HandleSeq& tlo, const VECT& glo,
                              ValueMap& valmap,
                              AtomSpace* scratch, bool silent,
                              Quotation quotation,
                              size_t tsz, size_t off) const
{
	size_t gsz = glo.size();

	// If we are here, there is a glob node in the pattern.  A glob can
	// match one or more atoms in a row. Thus, we have a more
	// complicated search ...
	size_t ip=0, jg=0;
	for (ip=0, jg=0; ip<tsz and jg<gsz; ip++, jg++)
	{
		Type ptype = tlo[ip]->get_type();
		if (GLOB_NODE == ptype)
		{
			HandleSeq glob_seq;
			Handle glob(tlo[ip]);
			// Globs at the end are handled differently than globs
			// which are followed by other stuff. So, is there
			// anything after the glob?
			Handle post_glob;
			bool have_post = false;
			if (ip+1 < tsz)
			{
				have_post = true;
				post_glob = tlo[ip+1];
			}

			// Match at least one.
			bool tc = extract(glob, glo[jg], valmap, scratch, silent, quotation);
			if (not tc) return false;

			glob_seq.push_back(glo[jg]);
			jg++;

			// Can we match more?
			while (tc and jg<gsz)
			{
				if (have_post)
				{
					// If the atom after the glob matches, then we are done.
					tc = extract(post_glob, glo[jg], valmap, scratch, silent, quotation);
					if (tc) break;
				}
				tc = extract(glob, glo[jg], valmap, scratch, silent, quotation);
				if (tc) glob_seq.push_back(glo[jg]);
				jg ++;
			}
			jg --;
			if (not tc)
			{
				return false;
			}

			// If we already have a value, the value must be identical.
			auto val = valmap.find(glob);
			if (valmap.end() != val)
			{
				// Have to have same arity and contents.
				if (val->second->is_atom())
				{
					const Handle& already = HandleCast(val->second);
					const HandleSeq& alo = already->getOutgoingSet();
					size_t asz = alo.size();
					if (asz != glob_seq.size()) return false;
					for (size_t i=0; i< asz; i++)
					{
						if (glob_seq[i] != alo[i]) return false;
					}
					return true;
				}
				else
				{
					throw RuntimeException(TRACE_INFO,
						"Globbing for Values not implemented! FIXME!");
				}
			}

			// If we are here, we've got a match. Record it.
			Handle glp(createLink(std::move(glob_seq), LIST_LINK));
			valmap.emplace(std::make_pair(glob, glp));
		}
		else
		{
			// If we are here, we are not comparing to a glob.
			if (not extract(tlo[ip], glo[jg], valmap, scratch, silent, quotation))
				return false;
		}
	}
	return (ip == tsz) and (jg == gsz);
}

template
bool FilterLink::glob_compare<HandleSeq>
                    (const HandleSeq&, const HandleSeq&,
                     ValueMap&, AtomSpace*, bool, Quotation,
                     size_t, size_t) const;

// ===============================================================

// XXX FIXME. LinkSignatureLink should be a C++ class,
// it can static-check the correct structure, and it can
// dynamic-compare the type correctly. Someday, not today.
static inline Type link_sig_kind(const Handle& termpat)
{
	TypeNodePtr tn(TypeNodeCast(termpat->getOutgoingAtom(0)));
	if (nullptr == tn)
		throw SyntaxException(TRACE_INFO,
			"Expecting first atom in LinkSignature to be a Type!");
	return tn->get_kind();
}

/// Recursive tree-compare-and-extract grounding values.
///
/// Compare the pattern tree `termpat` with the grounding tree `ground`.
/// If a variable in `termpat` corresponds with a variable in `ground`,
/// then add that correspondence pair to `valmap`. Type-checking is
/// performed during the match-up, so if the variable type does not
/// match the ground type, false is returned.  False is also returned
/// if the trees miscompare in other ways (mismatched link arity,
/// mis-matched atom type, two conflicting groundings for the same
/// variable).
///
/// Any executable terms in `ground` are executed prior to comparison.
/// XXX Is the above a good design choice? I dunno. It's the historical
/// choice. So it goes.
///
/// If false is returned, the contents of valmap are invalid. If true
/// is returned, valmap contains the extracted values.
///
bool FilterLink::extract(const Handle& termpat,
                         const ValuePtr& gnd,
                         ValueMap& valmap,
                         AtomSpace* scratch, bool silent,
                         Quotation quotation) const
{
	if (termpat == gnd) return true;

	ValuePtr vgnd(gnd);

	// Execute the proposed grounding term, first. Notice that this is
	// a "deep" execution, because there may have been lots of
	// non-executable stuff above us. Is this deep execution actually
	// a good idea? I dunno; this is an older design decision, motivated
	// by the URE. Is it a good design decision? I dunno. For now, there's
	// not enough experience to say. There is, however, a unit test to
	// check this behavior.
	Handle hgnd(HandleCast(gnd));
	if (hgnd and hgnd->is_executable())
	{
		vgnd = hgnd->execute(scratch, silent);
		if (nullptr == vgnd) return false;
	}

	// Let the conventional type-checker deal with complicated types.
	// LinkSignatureLinks might contain vars; deal with these below.
	if (termpat->is_type(TYPE_NODE) or
	    (termpat->is_type(TYPE_OUTPUT_LINK) and
	       (not termpat->is_type(LINK_SIGNATURE_LINK))))
		return value_is_type(termpat, vgnd);

	Type t = termpat->get_type();
	// If its a variable, then see if we know its value already;
	// If not, then record it.
	if (VARIABLE_NODE == t and 0 < _varset->count(termpat))
	{
		auto val = valmap.find(termpat);
		if (valmap.end() != val)
		{
			// If we already have a value, the value must be identical.
			return (val->second == vgnd);
		}

		// Check the type of the value.
		if (not _mvars->is_type(termpat, vgnd)) return false;

		// If we are here, everything looks good. Record and return.
		valmap.emplace(std::make_pair(termpat, vgnd));
		return true;
	}

	// Save quotation state before updating it
	Quotation quotation_cp;
	quotation.update(t);

	// Consume quotation
	if (quotation_cp.consumable(t))
		return extract(termpat->getOutgoingAtom(0), vgnd, valmap,
		               scratch, silent, quotation);

	if (GLOB_NODE == t and 0 < _varset->count(termpat))
	{
		// Check the type of the value.
		if (not _mvars->is_type(termpat, vgnd)) return false;
		return true;
	}

	// Special-case for ChoiceLinks in the body of the pattern.
	// This dangles one foot over the edge of a slippery slope,
	// of analyzing the body of the map and special-casing. Not
	// sure if this is a good idea, or a bad idea...
	if (CHOICE_LINK == t)
	{
		for (const Handle& choice : termpat->getOutgoingSet())
		{
			// Push and pop valmap each go-around; some choices
			// might partly work, and corrupt the map.
			ValueMap vcopy(valmap);
			if (extract(choice, vgnd, vcopy, scratch, silent, quotation))
			{
				valmap.swap(vcopy);
				return true;
			}
		}
		return false;
	}

	// If they are (non-variable) nodes, they must be identical.
	if (not termpat->is_link())
		return (termpat == vgnd);

	// Type of LinkSig is encoded in the first atom.
	Type lit = t;
	if (LINK_SIGNATURE_LINK == t)
		lit = link_sig_kind(termpat);

	// Whatever they are, the type must agree.
	if (lit != vgnd->get_type()) return false;

	// From here on out, we prepare to compare Links.
	const HandleSeq& tlo = termpat->getOutgoingSet();
	size_t tsz = tlo.size();
	size_t off = 0;
	if (LINK_SIGNATURE_LINK == t)
		{ tsz--; off = 1; }

	// If no glob nodes, just compare links side-by-side.
	if (0 == _globby_terms.count(termpat))
	{
		// This and the next block are nearly identical, except that
		// in the first, glo is std::vector<Handle> while the second
		// is std::vector<Value>. We could handle this with a template,
		// but the blocks are so short, that the template boilerplate
		// is longer than the block.
		if (vgnd->is_atom())
		{
			const HandleSeq& glo = HandleCast(vgnd)->getOutgoingSet();
			size_t gsz = glo.size();
			// If the sizes are mismatched, should we do a fuzzy match?
			if (tsz != gsz) return false;
			for (size_t i=0; i<tsz; i++)
			{
				if (not extract(tlo[i+off], glo[i], valmap, scratch, silent, quotation))
					return false;
			}
			return true;
		}

		// If we are here, vgnd is a LinkValue. Loop just like above,
		// except that the outgoing set is a vector of Values.
		const auto& glo = LinkValueCast(vgnd)->value();
		size_t gsz = glo.size();
		// If the sizes are mismatched, should we do a fuzzy match?
		if (tsz != gsz) return false;
		for (size_t i=0; i<tsz; i++)
		{
			if (not extract(tlo[i+off], glo[i], valmap, scratch, silent, quotation))
				return false;
		}
		return true;
	}

	Handle ground(HandleCast(vgnd));
	if (not ground)
		throw RuntimeException(TRACE_INFO,
			"Globbing for Values not implemented! FIXME!");

	const HandleSeq& glo = ground->getOutgoingSet();

	return glob_compare(tlo, glo, valmap, scratch, silent, quotation, tsz, off);
}

// ====================================================================

ValuePtr FilterLink::rewrite_one(const ValuePtr& vterm,
                                 AtomSpace* scratch, bool silent) const
{
	// temp hack
	Handle cterm(HandleCast(vterm));

	// See if the term passes pattern matching. If it does, the
	// side effect is that we get a grounding map as output.
	ValueMap valmap;
	if (not extract(_pattern->get_body(), vterm, valmap, scratch, silent))
		return Handle::UNDEFINED;

	// Special case for signatures. The extract already rejected
	// mis-matches, if any. Thus, we are done, here.
	const Handle& body(_pattern->get_body());
	if (body->is_type(TYPE_NODE) or
	    (body->is_type(TYPE_OUTPUT_LINK) and
	       (not body->is_type(LINK_SIGNATURE_LINK))))
		return vterm;

	// Special case for Values. This is minimalist, and does not support
	// RuleLink (see note below), which it should. It also assumes a
	// very minimalist structure for the LinkSignature kind, which is
	// also probably not correct. Someday, when users demand this, it
	// should be fixed.
	if (body->is_type(LINK_SIGNATURE_LINK))
	{
		if (valmap.size() == 1) return valmap.begin()->second;

		Type kind = link_sig_kind(body);
		if (LINK_VALUE == kind)
		{
			ValueSeq valseq;
			for (const Handle& var : _mvars->varseq)
			{
				auto valpair = valmap.find(var);
				valseq.emplace_back(valpair->second);
			}
			return createLinkValue(valseq);
		}

		// Fall through. Handle regular atoms.
	}

	// XXX FIXME. Everything below assumes we're working with Atoms not
	// Values, and that the RuleLink only has Atoms (not Values) in it's
	// rewrite. That's historically OK, but we do want to support the
	// general case of LinkValues, someday. Just not today.

	// Place the groundings into a sequence, for easy access.
	HandleSeq valseq;
	for (const Handle& var : _mvars->varseq)
	{
		auto valpair = valmap.find(var);

		// Can't ever happen.
		// if (valmap.end() == valpair) return Handle::UNDEFINED;

		valseq.emplace_back(HandleCast(valpair->second));
	}

	// Perform substitution, if it's a RuleLink.
	if (not _rewrite.empty())
	{
		HandleSeq rew;
		// Beta reduce, and execute. No type-checking during
		// beta-reduction; we've already done that, during matching.
		for (const Handle& impl : _rewrite)
		{
			Handle red(_mvars->substitute_nocheck(impl, valseq));
			if (red->is_executable())
				rew.emplace_back(HandleCast(red->execute(scratch, silent)));
			else
			{
				// Consume quotations.
				Type rty = red->get_type();
				if (LOCAL_QUOTE_LINK == rty or DONT_EXEC_LINK == rty)
					rew.emplace_back(red->getOutgoingAtom(0));
				else
					rew.emplace_back(red);
			}
		}

		// Fall through, use the same logic to finish up.
		valseq.swap(rew);
	}

	// Wrap up the result in a ListLink only if there is more
	// than one variable.
	size_t nv = valseq.size();
	if (1 < nv)
		return scratch->add_link(LIST_LINK, std::move(valseq));
	else if (1 == nv)
		return valseq[0];
	return Handle::UNDEFINED;
}

ValuePtr FilterLink::execute(AtomSpace* as, bool silent)
{
	Handle valh(_outgoing[1]);

	if (valh->is_executable())
	{
		ValuePtr vex = valh->execute();

		// XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,
		// then we should construct another Queue as the return value,
		// and perform filtering on-demand.
		if (vex->is_type(LINK_VALUE))
		{
			ValueSeq remap;
			for (const ValuePtr& vp : LinkValueCast(vex)->value())
			{
				ValuePtr mone = rewrite_one(vp, as, silent);
				if (nullptr != mone) remap.emplace_back(mone);
			}
			return createLinkValue(remap);
		}

		// If it is some other Value, we have no clue what to do with it.
		if (not vex->is_atom())
			return createLinkValue();

		// Fall through, if execution provided some Atom.
		valh = HandleCast(vex);
	}

	// Handle three different cases.
	// If there is a single Atom, apply the filter to the single Atom.
	// If there is a set of Atoms, apply the filter to the set.
	// If there is a list of Atoms, apply the filter to the list.
	Type argtype = valh->get_type();
	if (SET_LINK == argtype or LIST_LINK == argtype)
	{
		HandleSeq remap;
		for (const Handle& h : valh->getOutgoingSet())
		{
			Handle mone = HandleCast(rewrite_one(h, as, silent));
			if (nullptr != mone) remap.emplace_back(mone);
		}
		return as->add_link(argtype, std::move(remap));
	}

	// Its a singleton. Just remap that.
	Handle mone = HandleCast(rewrite_one(valh, as, silent));
	if (mone) return mone;

	// Avoid returning null pointer!
	// If we were given Atoms, assum the caller wants Atoms back.
	// Otherwise, avoid pollution and return VoidValue.
	// Actually, return an empty LinkValue; this allows downstream
	// pipelines to handle it just like any other LinkValue return.
	if (valh->is_atom())
		return as->add_link(SET_LINK);
	return createLinkValue();
}

DEFINE_LINK_FACTORY(FilterLink, FILTER_LINK)

/* ===================== END OF FILE ===================== */
