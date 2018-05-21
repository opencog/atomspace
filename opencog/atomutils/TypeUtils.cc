/*
 * TypeUtils.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  December 2015
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

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/proto/NameServer.h>

#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/VariableList.h>

#include "FindUtils.h"

#include "TypeUtils.h"

namespace opencog {

/* ================================================================= */
/**
 * Type checker.  Returns true if `val` is of type `deep`.
 */
bool value_is_type(const Handle& spec, const Handle& val)
{
	Handle deep(spec);

	Type valtype = val->get_type();
	Type dpt = deep->get_type();

	// If it's a user-defined type, replace by it's defintion.
	if (DEFINED_TYPE_NODE == dpt)
	{
		deep = DefineLink::get_definition(deep);
		dpt = deep->get_type();
	}

	// If it's a signature, unpack it now.
	if (SIGNATURE_LINK == dpt)
	{
		deep = deep->getOutgoingAtom(0);
		dpt = deep->get_type();
	}

	if (TYPE_NODE == dpt)
	{
		Type deeptype = TypeNodeCast(deep)->get_value();
		return (valtype == deeptype);
	}
	else if (TYPE_INH_NODE == dpt)
	{
		// Just like above, but allows derived types.
		Type deeptype = TypeNodeCast(deep)->get_value();
		return nameserver().isA(valtype, deeptype);
	}
	else if (TYPE_CO_INH_NODE == dpt)
	{
		// Just like above, but in the other direction.
		// That is, it allows base tyes.
		Type deeptype = TypeNodeCast(deep)->get_value();
		return nameserver().isA(deeptype, valtype);
	}
	else if (TYPE_CHOICE == dpt)
	{
		for (const Handle& choice : deep->getOutgoingSet())
		{
			if (value_is_type(choice, val)) return true;
		}
		return false;
	}
	else if (FUZZY_LINK == dpt)
	{
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");
	}

	// If it is a node, not a link, then it is a type-constant,
	// and thus must match perfectly.
	if (deep->is_node())
		return (deep == val);

	// If a link, then both must be same link type.
	if (valtype != dpt) return false;

	const HandleSeq& vlo = val->getOutgoingSet();
	const HandleSeq& dpo = deep->getOutgoingSet();
	size_t sz = dpo.size();

	// Both must be the same size...
	if (vlo.size() != sz) return false;

	// Unordered links are harder to handle...
	if (nameserver().isA(dpt, UNORDERED_LINK))
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");

	// Ordered links are compared side-by-side
	for (size_t i=0; i<sz; i++)
	{
		if (not value_is_type(dpo[i], vlo[i])) return false;
	}

	// If we are here, all checks must hav passed.
	return true;
}

/* ================================================================= */

/*
 * The implementation below feels awfully hacky and bug-prone,
 * but I don't see a better way.  Since below is essentially
 * a whizzy set-subset operation, one could go all formal operator
 * and etc. but more abstraction seems unlikely to make it better.
 */
static bool type_match_rec(const Handle& left_, const Handle& right_, bool toplevel)
{
	if (left_ == right_) return true;

	Handle left(left_);
	Type ltype = left->get_type();

	// If it's a user-defined type, replace by it's defintion.
	if (DEFINED_TYPE_NODE == ltype)
	{
		left = DefineLink::get_definition(left);
		ltype = left->get_type();
	}

	// Unpack the arrow; right must match left's input.
	if (ARROW_LINK == ltype)
	{
		left = left->getOutgoingAtom(0); // 0 == input
		ltype = left->get_type();
	}

	// If right is not a type, then just use value-check.
	// We can only do this at the top level; lower levels
	// can have value-like links (i.e. duck-types which
	// we have to type-interence).
	Type rtype = right_->get_type();
	if (toplevel and
	    TYPE_NODE != rtype and
	    TYPE_INH_NODE != rtype and
	    TYPE_CO_INH_NODE != rtype and
	    TYPE_CHOICE != rtype and
	    SIGNATURE_LINK != rtype and
	    DEFINED_TYPE_NODE != rtype and
	    ARROW_LINK != rtype)
	{
		return value_is_type(left, right_);
	}

	Handle right(right_);

	// If it's a user-defined type, replace by it's defintion.
	if (DEFINED_TYPE_NODE == rtype)
	{
		right = DefineLink::get_definition(right);
		rtype = right->get_type();
	}

	// Unpack the arrow; right's output must match left.
	if (ARROW_LINK == rtype)
	{
		right = right->getOutgoingAtom(1); // 1 == output
		rtype = right->get_type();
	}

	// Should be safe to unpack signatures now.
	// We must not do this earlier, it will mess up.
	if (SIGNATURE_LINK == ltype)
	{
		left = left->getOutgoingAtom(0);
		ltype = left->get_type();
	}

	if (SIGNATURE_LINK == rtype)
	{
		right = right->getOutgoingAtom(0);
		rtype = right->get_type();
	}

	// Exact matchees are always good.
	if (left == right) return true;

	// If left is a core type, right must be that type
	// (or a value, partly handled that above already, for
	// the top-level; here we do lower levels.
	if (TYPE_NODE == ltype)
	{
		return TypeNodeCast(left)->get_value() == rtype;
	}

	// Like above but allows derived tyes.
	if (TYPE_INH_NODE == ltype)
	{
		return nameserver().isA(rtype, TypeNodeCast(left)->get_value());
	}

	// Like above, but in the opposite direction: allows base types.
	if (TYPE_CO_INH_NODE == ltype)
	{
		return nameserver().isA(TypeNodeCast(left)->get_value(), rtype);
	}

	// If left is a type choice, right must match a choice.
	if (TYPE_CHOICE == ltype)
	{
		if (TYPE_CHOICE == rtype)
		{
			// Can everything in the right be found in the left?
			// If so, then we are OK.
			for (const Handle& rh : right->getOutgoingSet())
			{
				if (not type_match_rec(left, rh, false)) return false;
			}
			return true;
		}

		// Does one of the left choices match a right? Is so then good.
		for (const Handle& lh : left->getOutgoingSet())
		{
			if (type_match_rec(lh, right, false)) return true;
		}
		return false;
	}

	// At this point we expect both left an right to be links
	// that are not type-links, i.e. should be ordinary links,
	// e.g. ListLink or EvaluationLink.  Compare these side-by-side.
	if (ltype != rtype) return false;

	if (not left->is_link() or not right->is_link()) return false;

	// Unordered links are a pain in the butt.
	if (nameserver().isA(ltype, UNORDERED_LINK))
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");

	const HandleSeq& lout(left->getOutgoingSet());
	const HandleSeq& rout(right->getOutgoingSet());

	if (lout.size() != rout.size()) return false;

	for (size_t i=0; i< lout.size(); i++)
	{
		if (not type_match_rec(lout[i], rout[i], false)) return false;
	}

	return true;
}

bool type_match(const Handle& left_, const Handle& right_)
{
	return type_match_rec(left_, right_, true);
}

Handle type_compose(const Handle& left, const Handle& right)
{
	return Handle::UNDEFINED;
}

Handle filter_vardecl(const Handle& vardecl, const Handle& body)
{
	return filter_vardecl(vardecl, HandleSeq{body});
}

Handle filter_vardecl(const Handle& vardecl, const HandleSeq& hs)
{
	// Base cases

	if (not vardecl)
		// Return Handle::UNDEFINED to indicate that this variable
		// declaration is nonexistent.
		return Handle::UNDEFINED;

	Type t = vardecl->get_type();
	if (VARIABLE_NODE == t)
	{
		if (is_free_in_any_tree(hs, vardecl))
			return vardecl;
	}

	// Recursive cases

	else if (TYPED_VARIABLE_LINK == t)
	{
		Handle var = vardecl->getOutgoingAtom(0);
		Type t = var->get_type();
		if (t == VARIABLE_NODE and filter_vardecl(var, hs))
			return vardecl;
	}

	else if (VARIABLE_LIST == t)
	{
		HandleSeq subvardecls;
		HandleSet subvars;      // avoid duplicating variables
		for (const Handle& v : vardecl->getOutgoingSet())
		{
			if (filter_vardecl(v, hs) and subvars.find(v) == subvars.end()) {
				subvardecls.push_back(v);
				subvars.insert(v);
			}
		}
		if (subvardecls.empty() and get_free_variables(hs).empty())
			return Handle::UNDEFINED;
		if (subvardecls.size() == 1)
			return subvardecls[0];
		return Handle(createVariableList(subvardecls));
	}

	// If we're here we have failed to recognize vardecl as a useful
	// and well-formed variable declaration, so Handle::UNDEFINED is
	// returned.
	return Handle::UNDEFINED;
}

bool is_well_typed(Type t)
{
	return t != NOTYPE;
}

bool is_well_typed(const TypeSet& ts)
{
	for (Type t : ts)
		if (not is_well_typed(t))
			return false;
	return true;
}

Type type_intersection(Type lhs, Type rhs)
{
	NameServer& ns = nameserver();
	if (ns.isA(lhs, rhs))
		return lhs;
	if (ns.isA(rhs, lhs))
		return rhs;
	return NOTYPE;              // represent the bottom type
}

TypeSet type_intersection(Type lhs, const TypeSet& rhs)
{
	TypeSet res;
	// Distribute the intersection over the union type rhs
	for (Type rhst : rhs) {
		Type ty = type_intersection(lhs, rhst);
		if (ty != NOTYPE)
			res.insert(ty);
	}
	return res;
}

TypeSet type_intersection(const TypeSet& lhs,
                          const TypeSet& rhs)
{
	// Base cases
	if (lhs.empty())
		return rhs;
	if (rhs.empty())
		return lhs;

	// Recursive cases
	TypeSet res;
	for (Type ty : lhs) {
		TypeSet itr = type_intersection(ty, rhs);
		res.insert(itr.begin(), itr.end());
	}
	return res;
}

VariableListPtr gen_varlist(const Handle& h)
{
	HandleSet vars = get_free_variables(h);
	return createVariableList(HandleSeq(vars.begin(), vars.end()));
}

Handle gen_vardecl(const Handle& h)
{
	return Handle(gen_varlist(h));
}

VariableListPtr gen_varlist(const Handle& h, const Handle& vardecl)
{
	if (not vardecl)
		return gen_varlist(h);

	Type vardecl_t = vardecl->get_type();
	if (vardecl_t == VARIABLE_LIST)
		return VariableListCast(vardecl);

	OC_ASSERT(vardecl_t == VARIABLE_NODE
	          or vardecl_t == TYPED_VARIABLE_LINK);
	return createVariableList(vardecl);
}

Variables gen_variables(const Handle& h, const Handle& vardecl)
{
	return gen_varlist(h, vardecl)->get_variables();
}

Handle gen_vardecl(const Handle& h, const Handle& vardecl)
{
	if (not vardecl)
		return gen_vardecl(h);
	return vardecl;
}

Handle gen_vardecl(const HandleSeq& varlist)
{
	if (varlist.empty())
		return Handle::UNDEFINED;

	Handle vardecl;
	if (1 == varlist.size())
		return varlist[0];
	else
		return Handle(createVariableList(varlist));
}

} // ~namespace opencog

/* ===================== END OF FILE ===================== */
