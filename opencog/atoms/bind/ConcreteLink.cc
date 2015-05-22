/*
 * ConcreteLink.cc
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

#include "ConcreteLink.h"
#include "PatternUtils.h"
#include "VariableList.h"

using namespace opencog;


void ConcreteLink::init(void)
{
	_pat.redex_name = "anonymous ConcreteLink";
	extract_variables(_outgoing);
	unbundle_clauses(_body);
	validate_clauses(_varlist.varset, _pat.clauses);
	extract_optionals(_varlist.varset, _pat.clauses);

	// Locate the black-box clauses.
	HandleSeq concs, virts;
	unbundle_virtual(_varlist.varset, _pat.cnf_clauses,
	                 concs, virts, _pat.black);

	// Check to make sure the graph is connected. As a side-effect,
	// the (only) component is sorted into connection-order.
   std::vector<HandleSeq> comps;
   std::vector<std::set<Handle>> comp_vars;
	get_connected_components(_varlist.varset, _pat.cnf_clauses, comps, comp_vars);

	// Throw error if more than one component!
	check_connectivity(comps);

	// This puts them into connection-order.
	_pat.cnf_clauses = comps[0];

	make_connectivity_map(_pat.cnf_clauses);
}

// Special ctor for use by SatisfactionLink; we are given
// the pre-computed components.
ConcreteLink::ConcreteLink(const std::set<Handle>& vars,
                           const VariableTypeMap& typemap,
                           const HandleSeq& compo,
                           const std::set<Handle>& opts)
	: Link(CONCRETE_LINK, HandleSeq())
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

ConcreteLink::ConcreteLink(const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: Link(CONCRETE_LINK, hseq, tv, av)
{
	init();
}

ConcreteLink::ConcreteLink(const Handle& vars, const Handle& body,
                   TruthValuePtr tv, AttentionValuePtr av)
	: Link(CONCRETE_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

ConcreteLink::ConcreteLink(Type t, const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, hseq, tv, av)
{
	// Derived link-types have other init sequences
	if (CONCRETE_LINK != t) return;
	init();
}

ConcreteLink::ConcreteLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, CONCRETE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ConcreteLink, got %s", tname.c_str());
	}

	// Derived link-types have other init sequences
	if (CONCRETE_LINK != tscope) return;

	init();
}

/* ===================== END OF FILE ===================== */
