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

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atomutils/TypeUtils.h>

#include "BindLink.h"

using namespace opencog;

void BindLink::init(void)
{
	Type t = getType();
	if (not classserver().isA(t, BIND_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
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
	if (3 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at most two, got %d", sz);

	// If the outgoing set size is one, then there are no variable
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

Handle BindLink::substitute(const HandleMap& var2val, Handle vardecl) const
{
	// Perform alpha conversion over the variable declaration, if exist
	HandleSeq hs;
	Arity i = 0;
	if (_vardecl) {
		hs.push_back(substitute_vardecl(_vardecl, var2val));
		++i;
	}

	// Turn the map into a vector of new variable names/values
	HandleSeq values = _varlist.make_values(var2val);

	// Perform alpha conversion over the bodies
	for (; i < getArity(); ++i)
		hs.push_back(_varlist.substitute_nocheck(getOutgoingAtom(i), values));

	// Replace vardecl by the substituted version if any
	if (!vardecl and _vardecl)
		vardecl = hs[0];

	// Remove the optional variable declaration from hs
	if (_vardecl)
		hs.erase(hs.begin());

	// Filter vardecl
	vardecl = filter_vardecl(vardecl, hs);

	// Insert vardecl back in hs if defined
	if (vardecl)
		hs.insert(hs.begin(), vardecl);

	// Create the alpha converted scope link
	return classserver().factory(Handle(createLink(hs, getType())));
}

Handle BindLink::substitute_vardecl(const Handle& vardecl,
                                    const HandleMap& var2val) const
{
	if (not vardecl)
		return Handle::UNDEFINED;

	Type t = vardecl->getType();

	// Base cases

	if (t == VARIABLE_NODE) {
		auto it = var2val.find(vardecl);
		// Only substitute if the variable is substituted by another variable
		if (it != var2val.end() and it->second->getType() == VARIABLE_NODE)
			return it->second;
		return Handle::UNDEFINED;
	}

	// Recursive cases

	HandleSeq oset;

	if (t == VARIABLE_LIST) {
		for (const Handle& h : vardecl->getOutgoingSet()) {
			Handle nh = substitute_vardecl(h, var2val);
			if (nh)
				oset.push_back(nh);
		}
		if (oset.empty())
			return Handle::UNDEFINED;
	}
	else if (t == TYPED_VARIABLE_LINK) {
		Handle new_var = substitute_vardecl(vardecl->getOutgoingAtom(0),
		                                    var2val);
		if (new_var) {
			oset.push_back(new_var);
			oset.push_back(vardecl->getOutgoingAtom(1));
		} else return Handle::UNDEFINED;
	}
	else {
		OC_ASSERT(false, "Not implemented");
	}
	return classserver().factory(Handle(createLink(oset, t)));
}

/* ================================================================= */

DEFINE_LINK_FACTORY(BindLink, BIND_LINK)

/* ===================== END OF FILE ===================== */
