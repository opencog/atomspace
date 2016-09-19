/*
 * VariableList.cc
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

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/DefineLink.h>

#include "VariableList.h"

using namespace opencog;

void VariableList::validate_vardecl(const HandleSeq& oset)
{
	for (const Handle& h: oset)
	{
		Type t = h->getType();
		if (VARIABLE_NODE == t or GLOB_NODE == t)
		{
			_varlist.varset.insert(h);    // tree (unordered)
			_varlist.varseq.emplace_back(h); // vector (ordered)
		}
		else if (TYPED_VARIABLE_LINK == t)
		{
			get_vartype(h);
		}

		// Sigh. This UnquoteLink check is a bit of hackery. It can
		// happen, during pattern-recognition, that we are *searching*
		// for BindLink's/GetLink's, and need to construct a dummy
		// variable to match a variable list.  This dummy will be
		// unquoted first.  Its not unquoting an actual VariableList,
		// though, its just unquoting a dummy, and so there's nothing
		// there, its empty, there's nothing to do.  So just pass on
		// the whole she-bang.  See the `recog.scm` example for a
		// a real-world example of this happening.
		else if (UNQUOTE_LINK == t)
		{
			return;
		}
		else
			throw InvalidParamException(TRACE_INFO,
				"Expected a VariableNode or a TypedVariableLink, got: %s",
					classserver().getTypeName(t).c_str());
	}
	build_index();
}

VariableList::VariableList(const Handle& hvardecls,
                           TruthValuePtr tv, AttentionValuePtr av)
	: Link(VARIABLE_LIST,
	    // Either it is a VariableList, or its a naked variable, or
	    // its a typed variable.
	    hvardecls->getType() == VARIABLE_LIST ?
	          hvardecls->getOutgoingSet() : HandleSeq({hvardecls}),
	    tv, av)
{
	validate_vardecl(getOutgoingSet());
}

VariableList::VariableList(const HandleSeq& oset,
                           TruthValuePtr tv, AttentionValuePtr av)
	: Link(VARIABLE_LIST, oset, tv, av)
{
	validate_vardecl(oset);
}

VariableList::VariableList(Type t, const HandleSeq& oset,
                           TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, oset, tv, av)
{
	// derived classes have a different initialization order
	if (VARIABLE_LIST != t) return;
	validate_vardecl(oset);
}

VariableList::VariableList(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, VARIABLE_LIST))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a VariableList, got %s", tname.c_str());
	}

	// Dervided types have a different initialization sequence
	if (VARIABLE_LIST != tscope) return;
	validate_vardecl(_outgoing);
}

/* ================================================================= */
/**
 * Extract the variable type(s) from a TypedVariableLink
 *
 * The call is expecting htypelink to point to one of the two
 * following structures:
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeNode  "ConceptNode"
 *
 * or
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeChoice
 *          TypeNode  "ConceptNode"
 *          TypeNode  "NumberNode"
 *          TypeNode  "WordNode"
 *
 * or possibly types that are SignatureLink's or FuyzzyLink's or
 * polymorphic combinations thereof: e.g. the following is valid:
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeChoice
 *          TypeNode  "ConceptNode"
 *          SignatureLink
 *              InheritanceLink
 *                 PredicateNode "foobar"
 *                 TypeNode  "ListLink"
 *          FuzzyLink
 *              InheritanceLink
 *                 ConceptNode "animal"
 *                 ConceptNode "tiger"
 *
 * In either case, the variable itself is appended to "vset",
 * and the list of allowed types are associated with the variable
 * via the map "typemap".
 */
void VariableList::get_vartype(const Handle& htypelink)
{
	const HandleSeq& oset = htypelink->getOutgoingSet();
	if (2 != oset.size())
	{
		throw InvalidParamException(TRACE_INFO,
			"TypedVariableLink has wrong size, got %lu", oset.size());
	}

	Handle varname(oset[0]);
	Handle vartype(oset[1]);

	// If its a defined type, unbundle it.
	Type t = vartype->getType();
	if (DEFINED_TYPE_NODE == t)
	{
		vartype = DefineLink::get_definition(vartype);
		t = vartype->getType();
	}

	// The vartype is either a single type name, or a list of typenames.
	if (TYPE_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_value();
		if (vt != ATOM)  // Atom type is same as untyped.
		{
			std::set<Type> ts = {vt};
			_varlist._simple_typemap.insert({varname, ts});
		}
	}
	else if (TYPE_CHOICE == t)
	{
		std::set<Type> typeset;
		OrderedHandleSet deepset;
		OrderedHandleSet fuzzset;

		const HandleSeq& tset = vartype->getOutgoingSet();
		size_t tss = tset.size();
		for (size_t i=0; i<tss; i++)
		{
			Handle ht(tset[i]);
			Type var_type = ht->getType();
			if (TYPE_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_value();
				if (ATOM != vt) typeset.insert(vt);
			}
			else if (SIGNATURE_LINK == var_type)
			{
				const HandleSeq& sig = ht->getOutgoingSet();
				if (1 != sig.size())
					throw SyntaxException(TRACE_INFO,
						"Unexpected contents in SignatureLink\n"
						"Expected arity==1, got %s", vartype->toString().c_str());

				deepset.insert(ht);
			}
			else if (FUZZY_LINK == var_type)
			{
				const HandleSeq& fuz = ht->getOutgoingSet();
				if (1 != fuz.size())
					throw SyntaxException(TRACE_INFO,
						"Unexpected contents in FuzzyLink\n"
						"Expected arity==1, got %s", vartype->toString().c_str());

				fuzzset.insert(ht);
			}
			else
			{
				throw InvalidParamException(TRACE_INFO,
					"VariableChoice has unexpected content:\n"
					"Expected TypeNode, got %s",
					    classserver().getTypeName(ht->getType()).c_str());
			}
		}

		if (0 < typeset.size())
			_varlist._simple_typemap.insert({varname, typeset});
		if (0 < deepset.size())
			_varlist._deep_typemap.insert({varname, deepset});
		if (0 < fuzzset.size())
			_varlist._fuzzy_typemap.insert({varname, fuzzset});
	}
	else if (SIGNATURE_LINK == t)
	{
		const HandleSeq& tset = vartype->getOutgoingSet();
		if (1 != tset.size())
			throw SyntaxException(TRACE_INFO,
				"Unexpected contents in SignatureLink\n"
				"Expected arity==1, got %s", vartype->toString().c_str());

		OrderedHandleSet ts;
		ts.insert(vartype);
		_varlist._deep_typemap.insert({varname, ts});
	}
	else if (FUZZY_LINK == t)
	{
		const HandleSeq& tset = vartype->getOutgoingSet();
		if (1 != tset.size())
			throw SyntaxException(TRACE_INFO,
				"Unexpected contents in FuzzyLink\n"
				"Expected arity==1, got %s", vartype->toString().c_str());

		OrderedHandleSet ts;
		ts.insert(vartype);
		_varlist._fuzzy_typemap.insert({varname, ts});
	}
	else
	{
		throw SyntaxException(TRACE_INFO,
			"Unexpected contents in TypedVariableLink\n"
			"Expected type specifier (e.g. TypeNode, TypeChoice, etc.), got %s",
			classserver().getTypeName(t).c_str());
	}

	_varlist.varset.insert(varname);
	_varlist.varseq.emplace_back(varname);
}

/* ================================================================= */
/**
 * Validate variable declarations for syntax correctness.
 *
 * This will check to make sure that a set of variable declarations are
 * of a reasonable form. Thus, for example, a structure similar to the
 * below is expected.
 *
 *       VariableList
 *          VariableNode "$var0"
 *          VariableNode "$var1"
 *          TypedVariableLink
 *             VariableNode "$var2"
 *             TypeNode  "ConceptNode"
 *          TypedVariableLink
 *             VariableNode "$var3"
 *             TypeChoice
 *                 TypeNode  "PredicateNode"
 *                 TypeNode  "GroundedPredicateNode"
 *
 * As a side-effect, the variables and type restrictions are unpacked.
 */
void VariableList::validate_vardecl(const Handle& hdecls)
{
	// Expecting the declaration list to be either a single
	// variable, or a list of variable declarations
	Type tdecls = hdecls->getType();
	if (VARIABLE_NODE == tdecls or GLOB_NODE == tdecls)
	{
		_varlist.varset.insert(hdecls);
		_varlist.varseq.emplace_back(hdecls);
	}
	else if (TYPED_VARIABLE_LINK == tdecls)
	{
		get_vartype(hdecls);
	}
	else if (VARIABLE_LIST == tdecls)
	{
		// The list of variable declarations should be .. a list of
		// variables! Make sure its as expected.
		const HandleSeq& dset = hdecls->getOutgoingSet();
		validate_vardecl(dset);
	}
	else
	{
		throw InvalidParamException(TRACE_INFO,
			"Expected a VariableList holding variable declarations");
	}
	build_index();
}

/* ================================================================= */
/**
 * Build the index from variable name, to its ordinal number.
 * The index is needed for variable substitution, i.e. for the
 * substitute method below.  The specific sequence order of the
 * variables is essential for making substitution work.
 */
void VariableList::build_index(void)
{
	if (0 < _varlist.index.size()) return;
	size_t sz = _varlist.varseq.size();
	for (size_t i=0; i<sz; i++)
	{
		_varlist.index.insert(std::pair<Handle, unsigned int>(_varlist.varseq[i], i));
	}
}

std::string opencog::oc_to_string(const VariableListPtr& vlp)
{
	if (vlp == nullptr)
		return "nullvariablelist\n";
	else
		return oc_to_string(vlp->getHandle());
}

/* ===================== END OF FILE ===================== */
